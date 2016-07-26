tests - removing by index
testRemoveLast

| collection element result oldSize |
collection := self collectionWith5Elements .
element := collection last.
oldSize := collection size.

result := collection removeLast.
self assert: result = element .
self assert: collection size = (oldSize - 1).