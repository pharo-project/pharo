execute
execute
	| answer |
	answer := UIManager default confirm: 'Are you sure you want to unload ' , target name.
	answer
		ifTrue: [ target workingCopy unload ]