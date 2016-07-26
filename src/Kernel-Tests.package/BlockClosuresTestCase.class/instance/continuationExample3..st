examples
continuationExample3: aCollection

  " see comment in method continuationExample1:.
    The block named 'processor' takes a value with contiuation
    and a processing block. It creates a new value with continuation.
    Here we set up a chain of three values with continuation:
    one data source and two value processors.
    Again we use a collector to collect all values.  "

    | stream processor collector |

   stream := [:collection | | i localBlock |
                i := 1.
                localBlock :=
                    [  | current |
                     current := collection at: i.
                     i := i + 1.
                     Array with: current
                           with: (i<= collection size ifTrue: [localBlock]
                                                      ifFalse: [nil])
                    ].
             ].
 
  processor := 
            [:valueWithContinuation :activity | | localBlock |
                localBlock :=
                 [ | current |
                   current := valueWithContinuation value. 
                   Array with: (activity value: current first)
                         with: (current last notNil ifTrue: [localBlock])].
               localBlock
           ].

 collector := [:valueWithContinuation |  | oc |
                 oc := OrderedCollection new.
                 [ | local |
                  local := valueWithContinuation value.
                  oc add: local first.
                  local last notNil]
                 whileTrue: [].
                 oc.
               ].

  ^collector value: (processor value: (processor value: (stream value: aCollection)
                                                                     value: [:x | x * x])
                                            value: [:x | x - 10]).