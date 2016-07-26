updating
update: aSymbol
	| si |
	model ifNotNil: [
		aSymbol = #regexError
				ifTrue: [self findTextFieldMorph textColor: Color  red].
		aSymbol = #newFinding
				ifTrue: [^ self newFinding].
		aSymbol = #findPolicy
			ifTrue: [self findPolicyChanged].
		aSymbol = #findReplaceSelection 
			ifTrue: [self changed: #replaceEnabled.
				self state updateFindStartIndexForTextMorph: self model]].
	super update: aSymbol