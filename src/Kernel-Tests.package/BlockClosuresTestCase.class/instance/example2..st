examples
example2: anInteger

  " BlockClosuresTestCase new example2: 6"
  " to complicate the example1, we set up a dynamic reference chain that is
    used to dump all calls of facorial when recursion depth is maximal.
    The return value is an instance of orderedCollection, the trace. "

    | factorial trace |

   trace := OrderedCollection new.
   factorial :=
       [:x :dumper :trace2 |  | localDumper |
          localDumper := [ :collection |
                             collection add: x. 
                             dumper value: collection.].
            x = 1 ifTrue:
                    [localDumper value: trace2.
                     1]
                  ifFalse:
                    [(factorial value: x - 1
                                 value: localDumper
                                 value: trace2)* x.
                   ]
         ].
  factorial value: anInteger
             value: [ :collection | ]
             value: trace.
  ^trace