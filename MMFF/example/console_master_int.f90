subroutine console_master( mpi_info )

    use mod_mmff_mpi_info, only: T_mmff_mpi_info;

    implicit none;
    type(T_mmff_mpi_info), intent(in):: mpi_info;
    integer, pointer:: a(:), b(:), c(:);
    external:: fa, fb, null_int;
    integer:: i, n_work, hq1, hq2, ha1, ha2
    integer:: n_data;

    interface
        include '../src/mmff_console_interface.f90'
    end interface


    ! initialize
    call mmff_init( mpi_info );

    n_data = 100;

    ! send
    call mmff_reset( n_data, 1, hq1 );
    call mmff_create_data_int( a, fa, 's1', ha1 );

    call mmff_send_data( hq1 );
    call mmff_delete_data_int( a, ha1 );

    n_data = 100;
    ! receive
    call mmff_reset( n_data, 0, hq2 );
    call mmff_create_data_int( c, null_int, 'r1', ha2);
    call mmff_recv_data( hq2 );

    open(101, file='test.dat')
    do i = 1, 100
        write(101, '(i8)'), c(i);
    end do
    close(101)


    !   call analyze_result(a)
    call mmff_delete_data_int( c, ha2 );
    
    call mmff_final();

    print*, 'done!'

    return;

end subroutine console_master




subroutine fa( x, y )
    implicit none;
    integer:: x
    integer:: y

    y = x;

    return;
end subroutine fa




subroutine fb( x, y )
    implicit none;
    integer:: x
    integer:: y

    y = x**3d0

    return;
end subroutine fb




subroutine null_int( x, y )
    implicit none;
    integer, intent(in):: x
    integer:: y

    y = 0;

    return;
end subroutine null_int
