subroutine manage_and_send_data_dcp( i_arry )

    use mod_mmff_core_para, only: n_resc, i_rank, queu_info, arry_info
    use mod_log, only: write_log;

    implicit none;
    integer, intent(in):: i_arry
    integer:: i_queu, n_proc, i_proc, tag
    double complex, pointer:: send_data(:)

    interface

        subroutine create_mpi_data_dcp( n_proc, i_proc, n_assign, mpi_data)
            integer, intent(in):: n_proc, i_proc, n_assign(n_proc);
            double complex, pointer:: mpi_data(:)
        end subroutine create_mpi_data_dcp

        subroutine transfer_mpi_data_dcp( n_proc, i_proc, n_assign, data, mpi_data, flag )
            integer, intent(in):: n_proc, i_proc, flag
            integer, intent(in):: n_assign(n_proc);
            double complex, pointer:: data(:);
            double complex, pointer:: mpi_data(:)
        end subroutine transfer_mpi_data_dcp

        subroutine delete_mpi_data_dcp( mpi_data ) 
            double complex, pointer:: mpi_data(:);
        end subroutine delete_mpi_data_dcp

    end interface



    i_queu = arry_info(i_arry) % queu;

    if ( i_rank == 0 ) then

        n_proc = queu_info(i_queu) % n_proc;

        ! for a given data array, loop processes in use
        do i_proc = 1, n_proc

            call generate_tag( arry_info(i_arry) % id, i_proc, tag );

            call create_mpi_data_dcp( n_resc, i_proc, queu_info(i_queu) % a_task, send_data );
            call transfer_mpi_data_dcp( n_resc, i_proc, queu_info(i_queu) % a_task, &
                  arry_info(i_arry) % arry_dcp % ptr, send_data, 1 );
            call send_data_mpi_dcp( i_proc, &
                  queu_info(i_queu) % a_task(i_proc), &
                  send_data, &
                  tag, 1 );
            call delete_mpi_data_dcp( send_data );

        end do

    else
        ! for slave, there is no loop; the data array 
        ! can be directly send/recv without splitting

        call generate_tag( arry_info(i_arry) % id, i_rank, tag );

        call send_data_mpi_dcp( 0, &
              queu_info(i_queu) % n_data, &
              arry_info(i_arry) % arry_dcp % ptr, &
              tag, 0 );

    end if

    return;

end subroutine manage_and_send_data_dcp





subroutine manage_and_recv_data_dcp( i_arry )

    use mod_mmff_core_para, only: n_resc, i_rank, queu_info, arry_info
    use mod_log, only: write_log;


    implicit none;
    integer, intent(in):: i_arry
    integer:: i_proc, n_proc, i_queu, tag
    double complex, pointer:: recv_data(:);

    interface

        subroutine create_mpi_data_dcp( n_proc, i_proc, n_assign, mpi_data)
            integer, intent(in):: n_proc, i_proc, n_assign(n_proc);
            double complex, pointer:: mpi_data(:)
        end subroutine create_mpi_data_dcp

        subroutine transfer_mpi_data_dcp( n_proc, i_proc, n_assign, data, mpi_data, flag )
            integer, intent(in):: n_proc, i_proc, flag
            integer, intent(in):: n_assign(n_proc);
            double complex, pointer:: data(:);
            double complex, pointer:: mpi_data(:)
        end subroutine transfer_mpi_data_dcp

        subroutine delete_mpi_data_dcp( mpi_data ) 
            double complex, pointer:: mpi_data(:);
        end subroutine delete_mpi_data_dcp

    end interface


    i_queu = arry_info(i_arry) % queu;

    if( i_rank == 0 ) then

        n_proc = queu_info(i_queu) % n_proc;

        ! for a given data array, loop processes in use
        do i_proc = 1, n_proc

            call generate_tag( arry_info(i_arry) % id, i_proc, tag );

            call create_mpi_data_dcp( n_resc, i_proc, queu_info(i_queu) % a_task, recv_data );
            call recv_data_mpi_dcp( i_proc, &
                  queu_info(i_queu) % a_task(i_proc), &
                  recv_data, &
                  tag, 1 );
            call transfer_mpi_data_dcp( n_resc, i_proc, queu_info(i_queu) % a_task, &
                  arry_info(i_arry) % arry_dcp % ptr, recv_data, -1 );

            call delete_mpi_data_dcp( recv_data );

        end do


    else
        call generate_tag( arry_info(i_arry) % id, i_rank, tag );
        call recv_data_mpi_dcp( 0, &
              queu_info(i_queu) % n_data, &
              arry_info(i_arry) % arry_dcp % ptr, &
              tag, 0 );

    end if

end subroutine manage_and_recv_data_dcp
