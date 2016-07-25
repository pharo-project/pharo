accessing
properties: aMethodProperties
	"Set the method-properties of the receiver to aMethodProperties."
	self literalAt: self numLiterals - 1
		put: (aMethodProperties isEmpty
				ifTrue: [aMethodProperties selector]
				ifFalse: [aMethodProperties
							setMethod: self;
							yourself])