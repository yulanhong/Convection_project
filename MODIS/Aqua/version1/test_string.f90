        program test_string
                use stdlib_ascii, only :reverse
               character(len=5) :: a,b
                a='hello'
                b=reverse(a)
                print *, b,a(2:3)
        end
