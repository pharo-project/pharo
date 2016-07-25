adding
add: anAssociation
	| index element |
	index := self findElementOrNil: anAssociation key.
	element := array at: index.
	element == nil
		ifTrue: [self atNewIndex: index put: anAssociation]
		ifFalse: [element value: anAssociation value].
	^ anAssociation