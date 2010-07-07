! MMFF: MPI Framework by Fortran


! global initialization
subroutine mmff_init( mpi_info )
    use mod_mmff_mpi_info, only: T_mmff_mpi_info;
    use mod_mmff_core_para, only:  arry_count, queu_count, n_resc, i_rank

    implicit none;
    type(T_mmff_mpi_info):: mpi_info;
    integer:: i;

    arry_count = 0;
    queu_count = 0;
    n_resc = mpi_info % n_proc - 1;
    i_rank = mpi_info % i_rank;

    return;
end subroutine mmff_init



subroutine mmff_final( )
    use mod_mmff_core_para, only: queu_count, queu_info;

    implicit none;
    integer:: i;

    do i = 1, queu_count
        deallocate( queu_info(i) % a_task );
    end do

    return;
end subroutine mmff_final


! initialize a queue (several arrays with the same size compose a queue)
subroutine mmff_reset( n_data, b_verbose, h_queu )
    use mod_mmff_core_para, only: queu_count, arry_count, i_rank, n_resc, queu_info;

    implicit none;
    integer:: n_data, h_queu
    integer, intent(in):: b_verbose
    integer:: i;

    queu_count = queu_count + 1;

    allocate( queu_info(queu_count) % a_task( n_resc ) );

    if ( b_verbose == 1 ) then
        call assign_workload_with_log( n_data, n_resc, &
              queu_info(queu_count) % a_task );
    else
        call assign_workload( n_data, n_resc, &
              queu_info(queu_count) % a_task );
    end if

    queu_info(queu_count) % id = queu_count;
    queu_info(queu_count) % i_arry = arry_count + 1;
    queu_info(queu_count) % b_arry = 0;
    if( i_rank == 0 ) then
        queu_info(queu_count) % n_data = n_data;
        queu_info(queu_count) % n_proc = &
              count( queu_info(queu_count) % a_task > 0 );
    else
        queu_info(queu_count) % n_data = &
              queu_info(queu_count) % a_task(i_rank);
        queu_info(queu_count) % n_proc = 1;
    end if
    
    n_data = queu_info(queu_count) % n_data;
    h_queu = queu_info(queu_count) % id;

    return;
end subroutine mmff_reset






subroutine mmff_send_data( h_queu )
    
    use mod_mmff_core_para, only: arry_info, queu_info, queu_count, arry_count, &
          MPI_DOUBLE_PRECISION, MPI_DOUBLE_COMPLEX, MPI_INTEGER;

    implicit none;
    integer, intent(in):: h_queu
    integer:: i, i_arry_start, i_arry_end

    if( queu_info(h_queu) % i_arry == 0) return; ! no data array in the queue
    
    i_arry_start = queu_info(h_queu) % i_arry;
    if( h_queu == queu_count ) then
        i_arry_end = arry_count;
    else
        i_arry_end = queu_info(h_queu+1) % i_arry - 1;
    end if

    ! loop all the data arrays in a specified queue
    do i = i_arry_start, i_arry_end

        select case( arry_info(i) % type )
        case( MPI_DOUBLE_PRECISION )
            call manage_and_send_data_dbl( i );
        case( MPI_DOUBLE_COMPLEX )
            call manage_and_send_data_dcp( i );
        case( MPI_INTEGER )
            call manage_and_send_data_int( i );
        case default
        end select

    end do

    return;

end subroutine mmff_send_data





subroutine mmff_recv_data( h_queu )

    use mod_mmff_core_para, only: arry_info, queu_info, queu_count, arry_count, &
          MPI_DOUBLE_PRECISION, MPI_DOUBLE_COMPLEX, MPI_INTEGER;

    implicit none;
    integer, intent(in):: h_queu
    integer:: i, i_arry_start, i_arry_end

    if( queu_info(h_queu) % i_arry == 0) return; ! no data array in the queue
    
    i_arry_start = queu_info(h_queu) % i_arry;
    if( h_queu == queu_count ) then
        i_arry_end = arry_count;
    else
        i_arry_end = queu_info(h_queu+1) % i_arry - 1;
    end if


    do i = i_arry_start, i_arry_end

        select case( arry_info(i) % type )
        case( MPI_DOUBLE_PRECISION )
            call manage_and_recv_data_dbl( i );
        case( MPI_DOUBLE_COMPLEX )
            call manage_and_recv_data_dcp( i );
        case( MPI_INTEGER )
            call manage_and_recv_data_int( i );
        case default
        end select

    end do

    return;

end subroutine mmff_recv_data







subroutine mmff_create_data_dbl( a, f, str_a, h_arry )
    use mod_mmff_core_para, only: queu_count, arry_count, arry_info, queu_info, MPI_DOUBLE_PRECISION

    implicit none;
    double precision, pointer:: a(:);
    external:: f;
    integer:: i, h_arry;
    character(len=*):: str_a;


    arry_count = arry_count + 1;
    arry_info(arry_count) % id = arry_count;
    arry_info(arry_count) % type = MPI_DOUBLE_PRECISION;
    arry_info(arry_count) % queu = queu_count;
    arry_info(arry_count) % name = str_a;

    allocate( a( queu_info(queu_count) % n_data ) );

    do i = 1, queu_info(queu_count) % n_data
        call f( i, a(i) );
    end do

    if( associated(arry_info(arry_count) % arry_dbl % ptr) ) then
        nullify( arry_info(arry_count) % arry_dbl % ptr ); 
    end if
    arry_info(arry_count) % arry_dbl % ptr => a;

    arry_info(arry_count) % b_stat = 1;
    queu_info(queu_count) % b_arry = 1;

    h_arry = arry_info(arry_count) % id;

    return;
end subroutine mmff_create_data_dbl




subroutine mmff_delete_data_dbl( a, h_arry )
    use mod_mmff_core_para, only: arry_info

    implicit none;
    integer, intent(in):: h_arry;
    double precision, pointer:: a(:);

    ! deattach the pointer
    if( associated( arry_info(h_arry) % arry_dbl % ptr ) ) then
        nullify( arry_info(h_arry) % arry_dbl % ptr ); 
    end if
    ! free the heap
    deallocate( a );
    ! toggle the status
    arry_info(h_arry) % b_stat = 0;

    return;

end subroutine mmff_delete_data_dbl








subroutine mmff_create_data_dcp( a, f, str_a, h_arry )
    use mod_mmff_core_para, only: queu_count, arry_count, arry_info, queu_info, MPI_DOUBLE_COMPLEX

    implicit none;
    double complex, pointer:: a(:);
    external:: f;
    integer:: i, h_arry;
    character(len=*):: str_a;


    arry_count = arry_count + 1;
    arry_info(arry_count) % id = arry_count;
    arry_info(arry_count) % type = MPI_DOUBLE_COMPLEX;
    arry_info(arry_count) % queu = queu_count;
    arry_info(arry_count) % name = str_a;

    allocate( a( queu_info(queu_count) % n_data ) );

    do i = 1, queu_info(queu_count) % n_data
        call f( i, a(i) );
    end do

    if( associated(arry_info(arry_count) % arry_dcp % ptr) ) then
        nullify( arry_info(arry_count) % arry_dcp % ptr ); 
    end if
    arry_info(arry_count) % arry_dcp % ptr => a;

    arry_info(arry_count) % b_stat = 1;
    queu_info(queu_count) % b_arry = 1;

    h_arry = arry_info(arry_count) % id;

    return;
end subroutine mmff_create_data_dcp




subroutine mmff_delete_data_dcp( a, h_arry )
    use mod_mmff_core_para, only: arry_info

    implicit none;
    integer, intent(in):: h_arry;
    double complex, pointer:: a(:);

    ! deattach the pointer
    if( associated( arry_info(h_arry) % arry_dcp % ptr ) ) then
        nullify( arry_info(h_arry) % arry_dcp % ptr ); 
    end if
    ! free the heap
    deallocate( a );
    ! toggle the status
    arry_info(h_arry) % b_stat = 0;

    return;

end subroutine mmff_delete_data_dcp
