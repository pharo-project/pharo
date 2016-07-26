examples
example1: anInteger

  " this example is very simple. A named block recursively computes the factorial.
    The example tests whether the value of x is still available after the recursive call.
    Note that the recursive call precedes the multiplication. For the purpose of the test
    this is essential. (When you commute the factors, the example will work also in
    some system without block closures, but not in Pharo.) "

    | factorial |

   factorial := [:x | x = 1 ifTrue: [1]
                            ifFalse: [(factorial value: x - 1)* x]].
  ^ factorial value: anInteger


  