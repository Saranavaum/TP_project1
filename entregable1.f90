
program bst_test
    use :: alblist
    implicit none
    integer :: iostatus
    real(kind=8)::amount
    integer,parameter::length=15
    character(len=length):: deity1,deity2,transc,prep
    character(len=:),allocatable::tdeity1
    character(len=:),allocatable::tdeity2

    type(a_tree_node), pointer :: root

    real (kind=8):: total_credit,total_debit

    
    root => null()
    do
        read(*, *, iostat=iostatus) deity1,transc,amount,prep,deity2!iostat nos dice que cuando llegue al final de programa que pare
        if (iostatus /= 0) exit !iostat te lo da el read

        if (transc /= 'lent' .and. transc /= 'borrowed') then
            print'(a,a,a)', "Wrong transaction '",trim(transc),"'."

        else 
            tdeity1=trim(deity1)
            call bst_insert(root,tdeity1)

            tdeity2=trim(deity2)
            call bst_insert(root,tdeity2)!esto lo compara con el anterior se llama a si mismo
            !con los datos que teniamos antes

            call insert_amount(root, tdeity1, tdeity2, amount,transc)

            deallocate(tdeity1)!para limpiar la memoria es necesario
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

!¿por qué nos complicamos la vida haciendo el árbol para ordenar los nombres? 
!en vez de comparar nombre por nombre (orden n) lo ordenamos de orden log_2(n), siendo mucho más rapido



end program bst_test
