testing
isSameRevisionAs: aRGMethodDefinition
	"This method look for equality of the properties of the receiver"
	"Stamp is ignored as in many cases a method is saved without containing any change. However it appears as changed due to a different stamp"
	self flag: 'needs to be evaluated'.

	^(super isSameRevisionAs: aRGMethodDefinition)
		and: [ self selector = aRGMethodDefinition selector 
			and: [ self protocol = aRGMethodDefinition protocol
				and: [ self sourceCode = aRGMethodDefinition sourceCode ] ] ]