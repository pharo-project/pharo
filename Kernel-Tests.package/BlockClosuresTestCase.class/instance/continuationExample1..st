examples
continuationExample1: aCollection

  " see comment below.
    Here we simply collect the values of a value with continuation block "


     | streamCreator collector |

   streamCreator := [:collection | | i localBlock |
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
 

 collector := [:valueWithContinuation |  | oc |
                 oc := OrderedCollection new.
                 [ | local |
                  local := valueWithContinuation value.
                  oc add: local first.
                  local last notNil]
                 whileTrue: [].
                 oc.
               ].

  ^collector value: (streamCreator value: aCollection).

"The continuation examples are examples of a 'back to LISP' style.
These examples use blocks to process the elements of a collection in a
fashion that is similar to streaming.
The creator block creates a blocks that act like a stream. In the
following, this block is called a 'value with continuation block'.
When such a value with continuation block receives the message
value, it returns a Array of two elements, the value and the
continuation
 1. the next collection element
 2. a so-called continuation, which is either nil or a block
    that can return the next value with continuation.
    
To collect all elements of a value with continuation stream,
use the collector block. "
