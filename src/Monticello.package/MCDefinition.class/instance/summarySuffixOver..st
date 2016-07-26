printing
summarySuffixOver: previousDefinition
	^self source = previousDefinition source
		ifTrue: [ ' (source same but rev changed)' ]
		ifFalse: [ ' (changed)' ]