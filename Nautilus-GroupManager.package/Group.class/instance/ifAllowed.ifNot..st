private
ifAllowed: aBlock ifNot: anotherBlock

	^ self isReadOnly
		ifTrue: anotherBlock
		ifFalse: aBlock