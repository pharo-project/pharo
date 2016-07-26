utilities
allEncodingNames
	"TextConverter allEncodingNames"
	| encodingNames |
	encodingNames := Set new.
	self allSubclasses
		do: [:each | 
			| names | 
			names := each encodingNames.
			names notEmpty
				ifTrue: [encodingNames add: names first asSymbol]].
	^encodingNames