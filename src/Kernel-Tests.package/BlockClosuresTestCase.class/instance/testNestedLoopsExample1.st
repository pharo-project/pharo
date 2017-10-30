tests
testNestedLoopsExample1

   | arrays result |
   arrays := OrderedCollection new.
   arrays add: #(#a #b);
             add: #(1 2 3 4);
             add: #('w' 'x' 'y' 'z').
    result := OrderedCollection new.
    CollectionCombinator new
          forArrays:  arrays
          processWith: [:item |result addLast: item].
   self assert: ((self nestedLoopsExample: arrays) = result)
  