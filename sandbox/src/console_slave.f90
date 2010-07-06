subroutine console_slave( mpi_info )

    use mod_mmff_mpi_info, only: T_mmff_mpi_info;

    implicit none;
    type(T_mmff_mpi_info), intent(in):: mpi_info;
    double precision, pointer:: a(:), b(:), c(:);
    external:: fa, fb, null_dbl;
    integer::  i
    integer:: n_data, hq1, hq2, ha1, ha2

    interface
        include 'mmff_console_interface.f90'
    end interface

    

    call mmff_init( mpi_info );
    
    n_data = 20;
    call mmff_reset( n_data, 0, hq1 );
    call mmff_create_data_dbl( a, null_dbl, 's1', ha1 );
    call mmff_recv_data( hq1 );
   

    n_data = 20;
    call mmff_reset( n_data, 0, hq2 );
    call mmff_create_data_dbl( c, null_dbl, 'r1', ha2);
    
    ! -----------------------------------
    do i = 1, n_data
        c(i) = a(i) / 2d0;
    end do
    ! -----------------------------------

    call mmff_send_data( hq2 );
    call mmff_delete_data_dbl( a, ha1 );
    !   call analyze_result(a)
    call mmff_delete_data_dbl( c, ha2 );
    
    call mmff_final();

    return;

end subroutine console_slave
