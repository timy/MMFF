#define N_P0 10
subroutine console_master( mpi_info )

    use mod_mmff_mpi_info, only: T_mmff_mpi_info;

    implicit none;
    type(T_mmff_mpi_info), intent(in):: mpi_info;
    double precision, pointer:: px_0_(:), pz_0_(:), px_inf_(:), pz_inf_(:)
    double complex, pointer:: w_im_(:), w_re_(:)
    integer, pointer:: n_ts_in_p0_(:)
    integer, pointer:: a_task(:), n_ts(:)
    integer, allocatable:: n_p0_in_proc(:), n_ts_in_proc(:)
    external:: null_dbl, null_int, null_dcp
    integer:: first, last, i_proc, n_proc, n_all_ts
    integer:: h_q1, h_q2, h_q3
    integer:: h_a1, h_a2, h_a3, h_a4, h_a5, h_a6, h_a7, h_a8, h_a9
    integer:: n_data

    interface
        include 'mmff_console_interface.f90'
    end interface


    ! initialize
    call mmff_init( mpi_info );

    !* send px_0 and pz_0
    n_data = N_P0;
    call mmff_reset( n_data, 0, h_q1 );
    call mmff_create_data_dbl( px_0_, null_dbl, 'px_0', h_a1 );
    call mmff_create_data_dbl( pz_0_, null_dbl, 'pz_0', h_a2 );
    call mmff_send_data( h_q1 );
    call mmff_delete_data_dbl( px_0_, h_a1 );
    call mmff_delete_data_dbl( pz_0_, h_a2 );


    n_proc = mpi_info % n_proc - 1;
    allocate( n_p0_in_proc( n_proc ) );

    !* recv n_ts
    n_data = N_P0;
    call mmff_reset( n_data, 0, h_q2 );
    !! get how many p0 for each proc
    call mmff_get_task( h_q2, a_task );
    forall(i_proc=1:n_proc) n_p0_in_proc(i_proc) = a_task(i_proc); 
    call mmff_set_task( h_q2, a_task );
    !!
    call mmff_create_data_int( n_ts_in_p0_, null_int, 'n_ts', h_a3 );
    call mmff_recv_data( h_q2 );



    !* get how many traj for each proc
    allocate( n_ts_in_proc(n_proc) );
    do i_proc = 1, n_proc !loop over all processes
        first = sum( n_p0_in_proc(1:i_proc-1) ) + 1; !
        last = sum( n_p0_in_proc(1:i_proc) );
        n_ts_in_proc(i_proc) = sum( n_ts_in_p0_( first:last ) );
    end do
    n_all_ts = sum( n_ts_in_proc );



    !* broadcast the task of each proc to slaves
    call mmff_broadcast_master_int( n_proc, n_ts_in_proc );


    !* recv W, p0_inf
    n_data = n_all_ts; 
    call mmff_reset( n_data, 0, h_q3 );
    call mmff_get_task( h_q3, a_task );
    forall(i_proc=1:n_proc) a_task(i_proc) = &
          n_ts_in_proc(i_proc);
    call mmff_set_task( h_q3, a_task );
    call mmff_create_data_dbl( px_0_, null_dbl, 'px_0', h_a4 );
    call mmff_create_data_dbl( pz_0_, null_dbl, 'pz_0', h_a5 );
    call mmff_create_data_dbl( px_inf_, null_dbl, 'px_inf', h_a6 );
    call mmff_create_data_dbl( pz_inf_, null_dbl, 'pz_inf', h_a7 );
    call mmff_create_data_dcp( w_im_, null_dcp, 'w_im', h_a8 );
    call mmff_create_data_dcp( w_re_, null_dcp, 'w_re', h_a9 );
    call mmff_recv_data( h_q3 );

    !* data analysis and output

    !* finalization
    deallocate( n_p0_in_proc );
    deallocate( n_ts_in_proc );
!!$
    call mmff_delete_data_int( n_ts_in_p0_, h_a3 );
    call mmff_delete_data_dbl( px_0_, h_a4 );
    call mmff_delete_data_dbl( pz_0_, h_a5 );
    call mmff_delete_data_dbl( px_inf_, h_a6 );
    call mmff_delete_data_dbl( pz_inf_, h_a7 );
    call mmff_delete_data_dcp( w_im_, h_a8 );
    call mmff_delete_data_dcp( w_re_, h_a9 );

    call mmff_final();

    print*, 'Done!'
    return;

end subroutine console_master
