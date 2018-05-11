Extension { #name : #Character }

{ #category : #'*NECompletion' }
Character >> isCompletionCharacter [
	^ self isAlphaNumeric or: [ self = $: ]
]
