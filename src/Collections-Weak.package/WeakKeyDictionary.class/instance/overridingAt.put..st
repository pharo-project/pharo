accessing
overridingAt: key put: anObject
	"Set the value at key to be anObject.  If key is not found, create a new
	entry for key and set is value to anObject. Answer anObject.
	May override an association with key == nil"
	
	| index element |
	key isNil ifTrue:[^anObject].
	index := self scanForKeyOrNil: key.
	
	"There should always be room."
	index = 0 ifTrue: [ self error: 'No space left in dictionary' ].
	
	element := array at: index.
	element == nil
		ifTrue: [self atNewIndex: index put: (WeakKeyAssociation key: key value: anObject)]
		ifFalse: [
			element expired ifTrue: [ tally := tally + 1].
			element key: key.
			element value: anObject.
			self fullCheck.
		].
	^ anObject