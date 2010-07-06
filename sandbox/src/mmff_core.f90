module mod_mmff_mpi_info

    implicit none;

    type T_mmff_mpi_info
        integer:: i_rank;
        integer:: n_proc;
    end type T_mmff_mpi_info

end module mod_mmff_mpi_info



module mod_mmff_core_para

    parameter ( MAX_COUNT = 40 )
    parameter ( MAX_STR_LEN = 20 )
    parameter ( MPI_DOUBLE_PRECISION = 1275070495 )
    parameter ( MPI_DOUBLE_COMPLEX = 1275072546 )
    parameter ( MPI_INTEGER = 1275069467 )


    type T_ptr_arr_dbl
        double precision, pointer:: ptr(:);
    end type T_ptr_arr_dbl

    type T_ptr_arr_dcp
        double complex, pointer:: ptr(:);
    end type T_ptr_arr_dcp

    type T_ptr_arr_int
        integer, pointer:: ptr(:);
    end type T_ptr_arr_int


    type T_arry_info
        integer:: id;                                      ! id of the data array
        integer:: type;                                    ! data type of the array
        integer:: queu;                                    ! the queue where the array is
        character(len=MAX_STR_LEN):: name;                 ! the alias
        type( T_ptr_arr_dbl ):: arry_dbl;                  ! pointer to double
        type( T_ptr_arr_dcp ):: arry_dcp;                  ! pointer to double complex
        type( T_ptr_arr_int ):: arry_int;                  ! pointer to integer
        integer:: b_stat;                                  ! if the array is allocated
    end type T_arry_info


    type T_queu_info
        integer:: id;                                      ! id
        integer:: n_data;                                  ! the array size of the this queue
        integer:: i_arry;                                  ! the id of the first array in this queue
        integer:: b_arry;                                  ! if this queue contains the data array
        integer:: n_proc;                                  ! number of processes in use
        integer, allocatable:: a_task(:);                  ! number of workload for each process
    end type T_queu_info


    integer:: arry_count;                                  ! number of all arrays from start on
    integer:: queu_count;                                  ! number of all queues from start on
    integer:: n_resc;                                      ! total number of process resources
    integer:: i_rank;                                      ! current process id
    type(T_arry_info):: arry_info(MAX_COUNT);              ! information of the arrays in queues
    type(T_queu_info):: queu_info(MAX_COUNT);              ! information of the queues

end module mod_mmff_core_para



subroutine manage_and_send_data_dbl( i_arry )

    use mod_mmff_core_para, only: n_resc, i_rank, queu_info, arry_info
    use mod_log, only: write_log;

    implicit none;
    integer, intent(in):: i_arry
    integer:: i_queu, n_proc, i_proc, tag
    double precision, pointer:: send_data(:)

    interface

        subroutine create_mpi_data_dbl( n_proc, i_proc, n_assign, mpi_data)
            integer, intent(in):: n_proc, i_proc, n_assign(n_proc);
            double precision, pointer:: mpi_data(:)
        end subroutine create_mpi_data_dbl

        subroutine transfer_mpi_data_dbl( n_proc, i_proc, n_assign, data, mpi_data, flag )
            integer, intent(in):: n_proc, i_proc, flag
            integer, intent(in):: n_assign(n_proc);
            double precision, pointer:: data(:);
            double precision, pointer:: mpi_data(:)
        end subroutine transfer_mpi_data_dbl

        subroutine delete_mpi_data_dbl( mpi_data ) 
            double precision, pointer:: mpi_data(:);
        end subroutine delete_mpi_data_dbl

    end interface



    i_queu = arry_info(i_arry) % queu;

    if ( i_rank == 0 ) then

        n_proc = queu_info(i_queu) % n_proc;

        ! for a given data array, loop processes in use
        do i_proc = 1, n_proc

            call generate_tag( arry_info(i_arry) % id, i_proc, tag );

            call create_mpi_data_dbl( n_resc, i_proc, queu_info(i_queu) % a_task, send_data );
            call transfer_mpi_data_dbl( n_resc, i_proc, queu_info(i_queu) % a_task, &
                  arry_info(i_arry) % arry_dbl % ptr, send_data, 1 );
            call send_data_mpi_dbl( i_proc, &
                  queu_info(i_queu) % a_task(i_proc), &
                  send_data, &
                  tag, 1 );
            call delete_mpi_data_dbl( send_data );

        end do

    else
        ! for slave, there is no loop; the data array 
        ! can be directly send/recv without splitting

        call generate_tag( arry_info(i_arry) % id, i_rank, tag );

        call send_data_mpi_dbl( 0, &
              queu_info(i_queu) % n_data, &
              arry_info(i_arry) % arry_dbl % ptr, &
              tag, 0 );

    end if

    return;

end subroutine manage_and_send_data_dbl





subroutine manage_and_recv_data_dbl( i_arry )

    use mod_mmff_core_para, only: n_resc, i_rank, queu_info, arry_info
    use mod_log, only: write_log;


    implicit none;
    integer, intent(in):: i_arry
    integer:: i_proc, n_proc, i_queu, tag
    double precision, pointer:: recv_data(:);

    interface

        subroutine create_mpi_data_dbl( n_proc, i_proc, n_assign, mpi_data)
            integer, intent(in):: n_proc, i_proc, n_assign(n_proc);
            double precision, pointer:: mpi_data(:)
        end subroutine create_mpi_data_dbl

        subroutine transfer_mpi_data_dbl( n_proc, i_proc, n_assign, data, mpi_data, flag )
            integer, intent(in):: n_proc, i_proc, flag
            integer, intent(in):: n_assign(n_proc);
            double precision, pointer:: data(:);
            double precision, pointer:: mpi_data(:)
        end subroutine transfer_mpi_data_dbl

        subroutine delete_mpi_data_dbl( mpi_data ) 
            double precision, pointer:: mpi_data(:);
        end subroutine delete_mpi_data_dbl

    end interface


    i_queu = arry_info(i_arry) % queu;

    if( i_rank == 0 ) then

        n_proc = queu_info(i_queu) % n_proc;

        ! for a given data array, loop processes in use
        do i_proc = 1, n_proc

            call generate_tag( arry_info(i_arry) % id, i_proc, tag );

            write(*, '(a, i4)'), 'tag', tag

            call create_mpi_data_dbl( n_resc, i_proc, queu_info(i_queu) % a_task, recv_data );
            call recv_data_mpi_dbl( i_proc, &
                  queu_info(i_queu) % a_task(i_proc), &
                  recv_data, &
                  tag, 1 );
            call transfer_mpi_data_dbl( n_resc, i_proc, queu_info(i_queu) % a_task, &
                  arry_info(i_arry) % arry_dbl % ptr, recv_data, -1 );

            call delete_mpi_data_dbl( recv_data );

        end do


    else
        call generate_tag( arry_info(i_arry) % id, i_rank, tag );
        call recv_data_mpi_dbl( 0, &
              queu_info(i_queu) % n_data, &
              arry_info(i_arry) % arry_dbl % ptr, &
              tag, 0 );

    end if

end subroutine manage_and_recv_data_dbl





subroutine generate_tag( arry_id, i_proc, tag )

    implicit none;
    integer, intent(in):: arry_id, i_proc;
    integer:: tag;

    tag = arry_id * 1000 + i_proc

    return;
end subroutine generate_tag
