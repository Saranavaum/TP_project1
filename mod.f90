
module alblist
    implicit none

    ! Defining the variables
    type :: a_list_item
        type(a_tree_node), pointer :: deity
        real (kind=8):: amount
        type(a_list_item), pointer :: next ! This allows us to continue adding elements to the list
    end type a_list_item 

    type :: a_tree_node
        character(len=:), allocatable :: name
        type(a_list_item), pointer :: debit
        type(a_list_item), pointer :: credit
        type(a_tree_node), pointer :: left
        type(a_tree_node), pointer :: right
    end type a_tree_node

    contains

    ! Subroutine that creates the tree
    recursive subroutine bst_insert( node, newname )
        type(a_tree_node), pointer, intent(in out) :: node !intent tells the subroutine whether it can be modified or not
        character(len=:),allocatable,intent(in)     :: newname !read only

        if (.not.associated( node )) then !if it is not associated enter
            allocate(node) !create a node
            node = a_tree_node(name=newname, left=null(), right=null(),debit=null(), credit=null())!create the empty children
            print'(a,a)',newname,' has been added.'
        else if (llt(newname,node%name)) then !function compares alphabetically
            call bst_insert( node%left, newname )!enter if newname is alphabetically less than nodename
        else if (lgt(newname,node%name)) then
            call bst_insert( node%right, newname ) 
        else 
            continue
        end if
    end subroutine bst_insert

    !subroutine that prints the tree
    recursive subroutine bst_print( root )
        type(a_tree_node), pointer, intent(in out) :: root 

        if (associated(root)) then
            call bst_print(root%left)

            print '(a, a)', root%name, ':'
            if(associated(root%debit)) then
                print '(4x, a)', "debit" 
                call list_print(root%debit)
            end if

            if (associated(root%credit)) then
                print '(4x, a)', "credit" 
                call list_print(root%credit)
            end if

            call bst_print(root%right)
            
        end if

    end subroutine bst_print

    !subroutine that destroys the tree
    recursive subroutine bst_destroy( root )
        type(a_tree_node), pointer, intent(in out) :: root

        if (associated( root)) then
            call bst_destroy( root%left )
            call bst_destroy( root%right )
            call list_destroid( root%credit )
            call list_destroid( root%debit )
            deallocate(root)
        end if
    end subroutine bst_destroy


    !subroutine that adds elements to the list
    recursive subroutine list_insert( head, deity_node, newamount  )
        type(a_list_item), pointer, intent(in out) :: head 
        type(a_tree_node), pointer, intent(in) :: deity_node
        real (kind=8),intent(in) :: newamount
        type(a_list_item),pointer::aux

        if (.not.associated( head )) then 
            allocate(head)
            head = a_list_item(deity=null(), amount=newamount, next=null() )
            head%deity=>deity_node
        else if (llt(deity_node%name,head%deity%name)) then !menor
            !if it is minor it gets complicated, we want the head to point to the minor and the minor then to the current one, 
            !the allocate is necessary now because now we are not adding elements at the end, we need to add a node in between
            allocate(aux)
            aux=a_list_item(deity=null(), amount=newamount, next=null() )
            aux%deity=>deity_node
            aux%next=>head
            head=>aux

            !We do this to insert the node, we create an auxiliary node that points to the head 
            !(not head next) and we make the head point to the auxiliary


        else if (lgt(deity_node%name,head%deity%name)) then !head is my list on the site we are on now
            call list_insert( head%next, deity_node, newamount) 

        else
            head%amount=head%amount+newamount

        end if

    end subroutine list_insert



!subroutine that prints a list
    recursive subroutine list_print( head )
        type(a_list_item), pointer, intent(in out) :: head

        if (associated(head)) then
            print '(8x,a,1x,f0.2)', trim(head%deity%name),head%amount
            call list_print(head%next)
            
        end if

    end subroutine list_print


!subroutine that destroys a list
    recursive subroutine list_destroid( head )
    type(a_list_item), pointer, intent(in out) :: head

    if (associated(head)) then
        call list_destroid(head%next)
        deallocate(head)
    end if

    end subroutine list_destroid


!function that finds the node you give it and marks it with a pointer as output
    recursive function search_node(root,name)result(node_result)
        type(a_tree_node), pointer, intent(in out) :: root
        character(len=:),allocatable ,intent(in out):: name
        type(a_tree_node), pointer ::node_result

        if (llt(name,root%name)) then 
            node_result=>search_node( root%left, name ) 
        else if (lgt(name,root%name)) then
            node_result=>search_node( root%right, name ) 
        else 
            node_result=>root
        end if
    end function search_node

!subroutine that inserts amounts to the type of transaction that is indicated
    subroutine insert_amount(root, name_deity1, name_deity2, amount,transaction)
        type(a_tree_node), pointer, intent(in out)    :: root
        character(len=:), allocatable, intent(in out) :: name_deity1
        character(len=:), allocatable, intent(in out) :: name_deity2
        character(len=15), intent(in) :: transaction
        type(a_tree_node), pointer :: deity_node1
        type(a_tree_node), pointer :: deity_node2
        real(kind=8),intent(in) :: amount 

        deity_node1 => search_node(root, name_deity1)
        deity_node2 => search_node(root, name_deity2)


        if (trim(transaction) /= 'lent') then 
            call list_insert(deity_node1%credit, deity_node2, amount) 
            call list_insert(deity_node2%debit, deity_node1, amount) 
            
        else
            call list_insert(deity_node1%debit, deity_node2, amount) 
            call list_insert(deity_node2%credit, deity_node1, amount) 
        end if

    end subroutine insert_amount




!function that adds all the credit
    recursive function totalamount_credit(root) result(total_credit)
        type(a_tree_node),pointer,intent(in)::root
        type(a_list_item), pointer :: temp

        real(kind=8)::total_credit,total_left,total_right

        total_credit=0.0
        total_left=0.0
        total_right=0.0
        !The idea is that it adds by parts, everything that is to the right of the node,
        !to itself and to the left and that, in this way, it calls itself and adds everything
        if (associated(root)) then
            total_left=totalamount_credit(root%left)
            if (associated(root%credit)) then 
                temp => root%credit

                do while (associated(temp))
                    total_credit = total_credit + temp%amount
                    temp => temp%next
    
                end do 
            end if

            total_right=totalamount_credit(root%right)

            total_credit=total_left+total_right+total_credit

        end if
        return
    end function totalamount_credit


    !function that adds all the dedit
    recursive function totalamount_debit(root) result(total_debit)
        type(a_tree_node),pointer,intent(in)::root
        type(a_list_item), pointer :: temp

        real(kind=8)::total_debit,total_left,total_right
        total_debit=0.0
        total_left=0.0
        total_right=0.0

        if (associated(root)) then
            total_left=totalamount_debit(root%left)
            if (associated(root%debit)) then 
                temp => root%debit

                do while (associated(temp))
                    total_debit = total_debit + temp%amount
                    temp => temp%next
    
                end do 
            end if

            total_right=totalamount_debit(root%right)

            total_debit=total_left+total_right+total_debit

        end if
        return
    end function totalamount_debit

end module alblist
