#define N_P0 10
subroutine console_slave( mpi_info )

    use mod_mmff_mpi_info, only: T_mmff_mpi_info;

    implicit none;
    type(T_mmff_mpi_info), intent(in):: mpi_info;
    double precision, pointer:: px_0_(:), pz_0_(:), px_inf_(:), pz_inf_(:)
    double complex, pointer:: w_im_(:), w_re_(:)
    integer, pointer:: n_ts_in_p0_(:), n_ts_in_proc(:)
    integer, pointer:: a_task(:), n_ts(:)
    integer, allocatable:: px_0(:), pz_0(:)
    external:: null_dbl, null_int, null_dcp
    integer:: i, i_proc, i_p0, count
    integer:: h_q1, h_q2, h_q3
    integer:: h_a1, h_a2, h_a3, h_a4, h_a5, h_a6, h_a7, h_a8, h_a9
    integer:: n_data, n_proc, n_p0, n_all_ts

    interface
        include 'mmff_console_interface.f90'
    end interface


    ! initialize
    call mmff_init( mpi_info );

    !* recv px_0 and pz_0
    n_data = N_P0;
    call mmff_reset( n_data, 0, h_q1 );
    call mmff_create_data_dbl( px_0_, null_dbl, 'px_0', h_a1 );
    call mmff_create_data_dbl( pz_0_, null_dbl, 'pz_0', h_a2 );
    call mmff_recv_data( h_q1 );
    n_p0 = n_data;
    allocate( px_0(n_p0) );
    allocate( pz_0(n_p0) );
    do i_p0 = 1, n_p0
        px_0(i_p0) = px_0_(i_p0);
        pz_0(i_p0) = pz_0_(i_p0);
    end do
    call mmff_delete_data_dbl( px_0_, h_a1 );
    call mmff_delete_data_dbl( pz_0_, h_a2 );


    n_proc = mpi_info % n_proc - 1;


    !* send n_ts
    n_data = N_P0;
    call mmff_reset( n_data, 0, h_q2 );
    call mmff_create_data_int( n_ts_in_p0_, null_int, 'n_ts', h_a3 );
    !! -call local_minima to estimate the number of ts
    do i_p0 = 1, n_p0
        n_ts_in_p0_(i_p0) = mpi_info % i_rank + i_p0;
    end do

    !!
    call mmff_send_data( h_q2 );


    !* broadcast the task of each proc to slaves
    call mmff_broadcast_slave_int( n_ts_in_proc );
    n_all_ts = sum( n_ts_in_proc );

    !* send W, p0_inf
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
    !! calculate px_inf, pz_inf, w_re, w_im
    count = 0;
    do i_p0 = 1, n_p0
        do i = 1, n_ts_in_p0_(i_p0);
            count = count + 1;
            px_0_(count) = px_0(i_p0);
            pz_0_(count) = pz_0(i_p0);
        end do
    end do

    do i_p0 = 1, count
        px_inf_(i_p0) = 1d0;
        px_inf_(i_p0) = 2d0;
        w_im_(i_p0) = (1d0, 1d0);
        w_re_(i_p0) = (2d0, 2d0);
    end do


    !!
    call mmff_send_data( h_q3 );
!!$
!!$    !* data analysis and output
!!$
!!$    !* finalization
    deallocate( px_0 );
    deallocate( pz_0 );
    deallocate( n_ts_in_proc );
!!$
!!$
    call mmff_delete_data_int( n_ts_in_p0_, h_a3 );
    call mmff_delete_data_dbl( px_0_, h_a4 );
    call mmff_delete_data_dbl( pz_0_, h_a5 );
    call mmff_delete_data_dbl( px_inf_, h_a6 );
    call mmff_delete_data_dbl( pz_inf_, h_a7 );
    call mmff_delete_data_dcp( w_im_, h_a8 );
    call mmff_delete_data_dcp( w_re_, h_a9 );

    call mmff_final();


    return;

end subroutine console_slave


