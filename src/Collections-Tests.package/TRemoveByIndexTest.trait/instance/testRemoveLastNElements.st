tests - removing by index
testRemoveLastNElements

| collection  result oldSize elements |
collection := self collectionWith5Elements .
elements := {  (collection at: (4)). collection last. }.
oldSize := collection size.


result := (collection removeLast: 2).
self assert: result = elements.
self assert: collection size = (oldSize - 2).