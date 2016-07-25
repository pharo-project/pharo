tests - removing by index
testRemoveAt

| collection element result oldSize |
collection := self collectionWith5Elements .
element := collection at: 3.
oldSize := collection size.

result := collection removeAt: 3.
self assert: result = element .
self assert: collection size = (oldSize - 1).