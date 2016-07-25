printing
summarySuffixOver: previousDefinition
	| sourceChanged categoryChanged timeStampChanged |
	sourceChanged := self source ~= previousDefinition source.
	timeStampChanged := self timeStamp ~= previousDefinition timeStamp.
	categoryChanged := self category ~= previousDefinition category.
	sourceChanged | timeStampChanged | categoryChanged
		ifFalse: [ ^super summarySuffixOver: previousDefinition ].
	sourceChanged ifTrue: [
		^categoryChanged
			ifTrue: [ ' (changed and recategorized)' ]
			ifFalse: [ ' (changed)' ] ].
	timeStampChanged & categoryChanged
		ifTrue: [^ ' (recategorized and different time stamp)' ].
	^categoryChanged
		ifTrue: [ ' (only recategorized)' ]
		ifFalse: [ ' (only different time stamp)' ]
