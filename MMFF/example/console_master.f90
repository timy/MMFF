#define N_DATA 100
subroutine console_master( mpi_info )

    use mod_mmff_mpi_info, only: T_mmff_mpi_info;

    implicit none;
    type(T_mmff_mpi_info), intent(in):: mpi_info;
    double precision, pointer:: a(:), c(:);
    integer:: b(3);
    external:: fa, null_dbl;
    integer:: i, hq1, hq2, ha1, ha2
    integer:: n_data;

    interface
        include 'mmff_console_interface.f90'
    end interface


    ! initialize
    call mmff_init( mpi_info );

    ! send
    n_data = N_DATA;
    call mmff_reset( n_data, 0, hq1 );
    call mmff_create_data_dbl( a, null_dbl, 's1', ha1 );
    forall(i=1:N_DATA) a(i) = i*1d0;
    call mmff_send_data( hq1 );

    ! receive
    n_data = N_DATA;
    call mmff_reset( n_data, 0, hq2 );
    call mmff_create_data_dbl( c, null_dbl, 'r1', ha2);
    call mmff_recv_data( hq2 );

    open(101, file='test.dat')
    do i = 1, 100
        write(101, '((f15.8, 2x))'), c(i);
    end do
    close(101)

    b = (/ 1, 2, 3 /);
    call mmff_broadcast_master_int( 3, b );

    call mmff_delete_data_dbl( a, ha1 );
    call mmff_delete_data_dbl( c, ha2 );
    
    call mmff_final();

    print*, 'done!'

    return;

end subroutine console_master
