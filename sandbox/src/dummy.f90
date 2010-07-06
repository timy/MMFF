program main

    use mod_mmff_mpi_info, only: T_mmff_mpi_info;

    implicit none;

    type(T_mmff_mpi_info):: mpi_info;
    integer:: mpi_ierr;


    mpi_info % i_rank = 0;
    mpi_info % n_proc = 4;

    if ( mpi_info % i_rank == 0 ) then
    
        call console_master( mpi_info );

    else if ( mpi_info % i_rank > 0 ) then

        call console_slave( mpi_info );

    end if

end program main



subroutine send_data_mpi_dbl( i_proc, n_elem, send_data, tag, b_verbose )

    use mod_log, only: write_log

    implicit none;

    integer, intent(in):: i_proc, n_elem, tag, b_verbose;
    double precision:: send_data(n_elem);
    integer:: mpi_ierr;
    character(len=200):: text;

    
    write(*, '(a, 3(a,i4,2x))'), 'sending... ', 'i_proc ', i_proc, 'tag ', tag, 'n_elem ', n_elem

!!$    call mpi_send( send_data, n_elem, MPI_DOUBLE_PRECISION, &
!!$          i_proc, tag, MPI_COMM_WORLD, mpi_ierr );
!!$
!!$    if( b_verbose ) then
!!$        if ( mpi_ierr == MPI_SUCCESS ) then
!!$
!!$            write( text, '(a, i8, a, i8)' ), 'data ', tag, ' is sent to node ', i_proc; 
!!$            call write_log( text );
!!$
!!$        else 
!!$            write( text, '(a, i8, a, i8, a, i8)'), &
!!$                  'error from data ', tag, ' in process ', i_proc, &
!!$                  ': ', mpi_ierr;
!!$            call write_log( text );
!!$        end if
!!$    end if

    return;

end subroutine send_data_mpi_dbl



subroutine recv_data_mpi_dbl( i_proc, n_elem, recv_data, tag, b_verbose )

    use mod_log, only: write_log

    implicit none;

    integer, intent(in):: i_proc, n_elem, tag, b_verbose;
    double precision:: recv_data(n_elem);
    integer:: mpi_ierr, status(4);
    character(len=200):: text;

    write(*, '(a, 3(a,i4,2x))'), 'recving... ', 'i_proc ', i_proc, 'tag ', tag, 'n_elem ', n_elem

!!$    call mpi_recv( recv_data, n_elem, MPI_DOUBLE_PRECISION, i_proc, &
!!$          tag, MPI_COMM_WORLD, status, mpi_ierr );
!!$
!!$    if( b_verbose ) then
!!$        if ( mpi_ierr == MPI_SUCCESS ) then
!!$
!!$            write( text, '(a, i8, a, i8)' ), 'data ', tag, &
!!$                  ' is received from node ', i_proc; 
!!$            call write_log( text );
!!$        else
!!$            write( text, '(a, i8, a, i8, a, i8)'), &
!!$                  'error from data ', tag, ' in process ', i_proc, &
!!$                  ': ', mpi_ierr;
!!$            call write_log( text );
!!$        end if
!!$    end if
    return;

end subroutine recv_data_mpi_dbl





































































subroutine send_data_dcp_to_each_proc( tag )
    integer:: tag;
    return;
end subroutine send_data_dcp_to_each_proc



subroutine send_data_int_to_each_proc( tag )
    integer:: tag;
    return;
end subroutine send_data_int_to_each_proc



subroutine recv_data_dcp_from_each_proc( tag )
end subroutine recv_data_dcp_from_each_proc



subroutine recv_data_int_from_each_proc( tag )
end subroutine recv_data_int_from_each_proc


