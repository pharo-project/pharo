updating
update: aSymbol
	self textView ifNotNil: [
		aSymbol = #regexError
				ifTrue: [self regexError].
		aSymbol = #newFinding
				ifTrue: [^ self newFinding].
		aSymbol = #findPolicy
			ifTrue: [self findPolicyChanged]].
	super update: aSymbol