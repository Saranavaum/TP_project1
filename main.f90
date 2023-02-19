
program bst_test
    use :: alblist
    implicit none
    integer :: iostatus
    real(kind=8)::amount
    integer,parameter::length=15
    character(len=length):: deity1,deity2,transc,prep
    character(len=:),allocatable::tdeity1
    character(len=:),allocatable::tdeity2
    
    !Why do we complicate our lives making the tree to order the names?
    !Instead of comparing name by name (order n) we order it in order log_2(n), being much faster

    type(a_tree_node), pointer :: root

    real (kind=8):: total_credit,total_debit

    
    root => null()
    do
        read(*, *, iostat=iostatus) deity1,transc,amount,prep,deity2!iostat tells us that when it reaches the end of the program to stop
        if (iostatus /= 0) exit 

        if (transc /= 'lent' .and. transc /= 'borrowed') then
            print'(a,a,a)', "Wrong transaction '",trim(transc),"'."

        else 
            tdeity1=trim(deity1)
            call bst_insert(root,tdeity1)

            tdeity2=trim(deity2)
            call bst_insert(root,tdeity2)!this compares it with the previous one it calls itself with the data we had before

            call insert_amount(root, tdeity1, tdeity2, amount,transc)

            deallocate(tdeity1)!to clear the memory it is necessary
            deallocate(tdeity2)
        end if
    
    end do

print*
call bst_print(root)

total_debit=totalamount_debit(root)
total_credit=totalamount_credit(root)
print*
print '(a, 2x, f0.2)', 'Net debit:', total_debit
print '(a, 1x, f0.2)', 'Net credit:', total_credit

call bst_destroy(root)

end program bst_test
