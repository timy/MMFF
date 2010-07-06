program main

    use mod_mmff_mpi_info, only: T_mmff_mpi_info;

    implicit none;
    include 'mpif.h'

    type(T_mmff_mpi_info):: mpi_info;
    integer:: mpi_ierr;

    call MPI_init( mpi_ierr );
    call MPI_comm_rank( MPI_COMM_WORLD, mpi_info % i_rank, mpi_ierr );
    call MPI_comm_size( MPI_COMM_WORLD, mpi_info % n_proc, mpi_ierr );


    if ( mpi_info % i_rank == 0 ) then
    
        call console_master( mpi_info );

    else if ( mpi_info % i_rank > 0 ) then

        call console_slave( mpi_info );

    end if

    call MPI_finalize( mpi_ierr );


end program main
