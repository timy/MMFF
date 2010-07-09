subroutine console_slave( mpi_info )

    use mod_mmff_mpi_info, only: T_mmff_mpi_info;

    implicit none;
    type(T_mmff_mpi_info), intent(in):: mpi_info;
    integer, pointer:: a(:), b(:), c(:);
    external:: fa, fb, null_int;
    integer::  i
    integer:: n_data, hq1, hq2, ha1, ha2

    interface
        include '../src/mmff_console_interface.f90'
    end interface

    

    call mmff_init( mpi_info );
    
    n_data = 100;
    call mmff_reset( n_data, 0, hq1 );
    call mmff_create_data_int( a, null_int, 's1', ha1 );
    call mmff_recv_data( hq1 );
   
    print*, a;
    n_data = 100;
    call mmff_reset( n_data, 0, hq2 );
    call mmff_create_data_int( c, null_int, 'r1', ha2);
    
    ! -----------------------------------
    do i = 1, n_data
        c(i) = a(i) * 2;
    end do
    ! -----------------------------------

    call mmff_send_data( hq2 );
    call mmff_delete_data_int( a, ha1 );
    !   call analyze_result(a)
    call mmff_delete_data_int( c, ha2 );
    
    call mmff_final();

    return;

end subroutine console_slave
