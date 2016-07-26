*Tools-Debugger
canonicalArgumentName

	| prefix |
	prefix := self name first isVowel
		ifTrue: [ 'an' ]
		ifFalse: [ 'a' ].
	^ prefix, self name.