Extension { #name : 'OCProgramNode' }

{ #category : '*Deprecated15' }
OCProgramNode >> formattedCodeIn: context [

	self
		deprecated: 'Ask the formatter: aClass formatter format: aNode'
		in: #Pharo15
		transformWith: '`@rcv formattedCodeIn: `@context' -> '`@context formatter format: `@rcv'.
	^ context formatter format: self
]
