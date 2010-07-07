subroutine generate_tag( arry_id, i_proc, tag )

    implicit none;
    integer, intent(in):: arry_id, i_proc;
    integer:: tag;

    tag = arry_id * 1000 + i_proc

    return;
end subroutine generate_tag
