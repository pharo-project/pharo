*NECompletion
isCompletionCharacter
	^ self isAlphaNumeric or: [ self = $: ]