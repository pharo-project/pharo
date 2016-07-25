testing
testFirstAndLastLink
	| list firstLink secondLink |
	list := DoubleLinkedList new.
	self should: [ list firstLink ] raise: CollectionIsEmpty. 
	self should: [ list lastLink ] raise: CollectionIsEmpty. 
	self should: [ list first ] raise: CollectionIsEmpty. 
	self should: [ list last ] raise: CollectionIsEmpty. 
	firstLink := list add: #one.
	self assert: list firstLink equals: firstLink.
	self assert: list first equals: list last.
	self assert: list firstLink equals: list lastLink.
	self assert: list firstLink value equals: #one.
	secondLink := list add: #two.
	self assert: list firstLink equals: firstLink.
	self assert: list lastLink equals: secondLink.
	self deny: list first = list last.
	self deny: list firstLink = list lastLink.
	self assert: list firstLink value equals: #one.
	self assert: list lastLink value equals: #two.

