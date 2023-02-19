
module alblist
    implicit none

    !definición de las variables
    type :: a_list_item
        type(a_tree_node), pointer :: deity
        real (kind=8):: amount
        type(a_list_item), pointer :: next!esto nos permite seguir añadiendo cosas a la lista
    end type a_list_item!es como el left y rigth

    type :: a_tree_node
        character(len=:), allocatable :: name
        type(a_list_item), pointer :: debit
        type(a_list_item), pointer :: credit
        type(a_tree_node), pointer :: left
        type(a_tree_node), pointer :: right
    end type a_tree_node

    contains

    !subrutina que crea el arbol
    recursive subroutine bst_insert( node, newname )
        type(a_tree_node), pointer, intent(in out) :: node !intent le dice a la subrutina si se puede modificar o no
        character(len=:),allocatable,intent(in)     :: newname !solo lectura

        if (.not.associated( node )) then !si no esta asociado entra
            allocate(node)!crea un nodo
            node = a_tree_node(name=newname, left=null(), right=null(),debit=null(), credit=null())!crea los hijos vacios
            print'(a,a)',newname,' has been added.'
        else if (llt(newname,node%name)) then !la funcion compara de forma alfabetica
            call bst_insert( node%left, newname ) !entra si newname es menor alfabeticamente que nodename
        else if (lgt(newname,node%name)) then
            call bst_insert( node%right, newname ) 
        else 
            continue
        end if
    end subroutine bst_insert

    !subrutina que imprime el arbol
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

    !subrutina que destruye el arbol
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


    !subrutina que añade elementos a la lista
    recursive subroutine list_insert( head, deity_node, newamount  )
        type(a_list_item), pointer, intent(in out) :: head !intent le dice a la subrutina si se puede modificar o no
        type(a_tree_node), pointer, intent(in) :: deity_node
        real (kind=8),intent(in) :: newamount
        type(a_list_item),pointer::aux

        if (.not.associated( head )) then !si no esta asociado entra
            allocate(head)
            head = a_list_item(deity=null(), amount=newamount, next=null() )!crea los hijos vacios
            head%deity=>deity_node
        else if (llt(deity_node%name,head%deity%name)) then !esto es menor
            !si es menor se complica queremos que el head señale al menor y el menor luego al de ahora
            !el allocate es necesario ahora por que ahora no estamos añadiendo elementos al final
            !necesitamos añadir entre medias un nodo
            allocate(aux)
            aux=a_list_item(deity=null(), amount=newamount, next=null() )
            aux%deity=>deity_node
            !creamos el nuevo nodo temporal para poder señalar e insertarlo
            !queremos que el head apunte
            aux%next=>head
            head=>aux

            !esto lo hacemos para insertar el nodo ,creamos un nodo auxiliar que apunta al head(no al head next)
            !y hacemos que el head apunte al auxiliar


        else if (lgt(deity_node%name,head%deity%name)) then !head es mi lista en sitio en elque estamos ahora
            call list_insert( head%next, deity_node, newamount) !el otro es el que quieres insertar
        !esto si es mayor

        else
            head%amount=head%amount+newamount

        end if

    end subroutine list_insert



!subrutina que imprime una lista
    recursive subroutine list_print( head )
        type(a_list_item), pointer, intent(in out) :: head

        if (associated(head)) then
            print '(8x,a,1x,f0.2)', trim(head%deity%name),head%amount
            call list_print(head%next)
            
        end if

    end subroutine list_print


!subrutina que destruye una lista
    recursive subroutine list_destroid( head )
    type(a_list_item), pointer, intent(in out) :: head

    if (associated(head)) then
        call list_destroid(head%next)
        deallocate(head)
    end if

    end subroutine list_destroid


!funcion que encuentra el nodo que le das y lo señala con un puntero como salida
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

!subrutina que inserta cantidades al tipo de transaccion que se le indica
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




!función que suma todo el crédito
    recursive function totalamount_credit(root) result(total_credit)
        type(a_tree_node),pointer,intent(in)::root
        type(a_list_item), pointer :: temp

        real(kind=8)::total_credit,total_left,total_right

        total_credit=0.0
        total_left=0.0
        total_right=0.0
        !la idea esque sume por partes, todo lo que hay a la derecha del nodo, a sí mismoy a la izquierda y que
        !asi se vaya llamando a si mismo y sumandolo todo
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