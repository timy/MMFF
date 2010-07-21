#define N_DATA 100
subroutine console_slave( mpi_info )

    use mod_mmff_mpi_info, only: T_mmff_mpi_info;

    implicit none;
    type(T_mmff_mpi_info), intent(in):: mpi_info
    double precision, pointer:: a(:), c(:)
    integer, pointer:: b(:)
    integer, pointer:: a_task(:), brdcast_a(:)
    external:: fa, fb, null_dbl
    integer::  i
    integer:: n_data, hq1, hq2, ha1, ha2

    interface
        include 'mmff_console_interface.f90'
    end interface

    ! initialize
    call mmff_init( mpi_info );

    ! recv
    n_data = N_DATA;
    call mmff_reset( n_data, 0, hq1 );
    call mmff_create_data_dbl( a, null_dbl, 's1', ha1 );
    call mmff_recv_data( hq1 );

    ! send
    n_data = N_DATA;
    call mmff_reset( n_data, 0, hq2 );
    call mmff_create_data_dbl( c, null_dbl, 'r1', ha2);
    
    forall(i=1:n_data) c(i) = a(i) * 2d0;

    call mmff_send_data( hq2 );


    call mmff_broadcast_slave_int( b );
    print*, mpi_info%i_rank, b;

    call mmff_delete_data_dbl( a, ha1 );
    call mmff_delete_data_dbl( c, ha2 );
    
    call mmff_final();

    print*, 'done!'

    return;

end subroutine console_slave
