tests - removing by index
testRemoveFirstNElements

| collection elements result oldSize |
collection := self collectionWith5Elements .
elements := {collection first. collection at:2}.
oldSize := collection size.

result := collection removeFirst: 2.
self assert: result = elements .
self assert: collection size = (oldSize - 2).