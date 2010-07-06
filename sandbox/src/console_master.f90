subroutine console_master( mpi_info )

    use mod_mmff_mpi_info, only: T_mmff_mpi_info;

    implicit none;
    type(T_mmff_mpi_info), intent(in):: mpi_info;
    double precision, pointer:: a(:), b(:), c(:);
    external:: fa, fb, null_dbl;
    integer:: i, n_work, hq1, hq2, ha1, ha2
    integer:: n_data;

    interface
        include 'mmff_console_interface.f90'
    end interface


    ! initialize
    call mmff_init( mpi_info );

    n_data = 20;

    ! send
    call mmff_reset( n_data, 1, hq1 );
    call mmff_create_data_dbl( a, fa, 's1', ha1 );
    call mmff_send_data( hq1 );
    call mmff_delete_data_dbl( a, ha1 );

    n_data = 20;
    ! receive
    call mmff_reset( n_data, 0, hq2 );
    call mmff_create_data_dbl( c, null_dbl, 'r1', ha2);
    call mmff_recv_data( hq2 );

    open(101, file='test.dat')
    do i = 1, 100
        write(101, '(f15.8)'), c(i);
    end do
    close(101)


    !   call analyze_result(a)
    call mmff_delete_data_dbl( c, ha2 );
    
    call mmff_final();

    print*, 'done!'

    return;

end subroutine console_master




subroutine fa( x, y )
    implicit none;
    integer:: x
    double precision:: y

    y = (1d0*x)**2d0

    return;
end subroutine fa




subroutine fb( x, y )
    implicit none;
    integer:: x
    double precision:: y

    y = (1d0*x)**3d0

    return;
end subroutine fb




subroutine null_dbl( x, y )
    implicit none;
    integer, intent(in):: x
    double precision:: y

    y = 0d0;

    return;
end subroutine null_dbl
