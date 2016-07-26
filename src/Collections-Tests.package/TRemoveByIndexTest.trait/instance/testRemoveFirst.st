tests - removing by index
testRemoveFirst

| collection element result oldSize |
collection := self collectionWith5Elements .
element := collection first.
oldSize := collection size.

result := collection removeFirst.
self assert: result = element .
self assert: collection size = (oldSize - 1).