Extension { #name : 'CompiledMethod' }

{ #category : '*Monticello-Model' }
CompiledMethod >> asMCMethodDefinition [
	"Creates a MCMethodDefinition from the receiver"

	^ MCMethodDefinition
		  className: self methodClass instanceSide name
		  classIsMeta: self isClassSide
		  selector: self selector asSymbol
		  category: self protocolName
		  sourcePointer: self sourcePointer
		  source: self sourceCode
]
