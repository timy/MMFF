
subroutine mmff_init( mpi_info )
    use mod_mmff_mpi_info, only: T_mmff_mpi_info
    type(T_mmff_mpi_info):: mpi_info;
end subroutine mmff_init



subroutine mmff_reset( n_data, flag, h_queu )
    integer, intent(in)::  n_data, flag, h_queu
end subroutine mmff_reset



subroutine mmff_create_data_dbl( a, f, str, h_arry )
    double precision, pointer:: a(:)
    external:: f;
    character(len=*):: str;
    integer, intent(in):: h_arry;
end subroutine mmff_create_data_dbl



subroutine mmff_delete_data_dbl( a, h_arry )
    double precision, pointer:: a(:)
    integer, intent(in):: h_arry
end subroutine mmff_delete_data_dbl



subroutine mmff_send_data( h_queu )
    integer, intent(in):: h_queu;
end subroutine mmff_send_data



subroutine mmff_recv_data( h_queu )
    integer, intent(in):: h_queu;
end subroutine mmff_recv_data
