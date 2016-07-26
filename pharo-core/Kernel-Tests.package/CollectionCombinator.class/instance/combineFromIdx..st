operating
combineFromIdx: myIdx

   "  this method is recursive. Recursion runs from values 1 to  collectionOfArrays size  of parameter myIdx. Each time it is called, this method has the responsiblity to provide all possible values for one index position of the result tuples. That index position is given by the value of  myIdx."

   (collectionOfArrays at: myIdx) do:
     [:item |
       buffer at: myIdx put: item.
       myIdx = collectionOfArrays size
         ifTrue: [resultProcessingBlock value: buffer shallowCopy]
         ifFalse: [self combineFromIdx: myIdx + 1]
    ].

  " The buffer is a shared object and its contents are later changed. It is therefore necessary to make a copy. "