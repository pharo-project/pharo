Extension { #name : 'CompiledMethod' }

{ #category : '*Monticello-Model' }
CompiledMethod >> asMCMethodDefinition [
	"Creates a MCMethodDefinition from the receiver"

	^ MCMethodDefinition
		  className: self methodClass instanceSide name
		  classIsMeta: self isClassSide
		  selector: self selector
		  category: self protocolName
		  timeStamp: self stamp
		  source: self sourceCode
]

{ #category : '*Monticello-Model' }
CompiledMethod >> basicAsMCMethodDefinition [

	"Creates a MCMethodDefinition from the receiver"

	^ MCMethodDefinition
		  className: self methodClass instanceSide name
		  classIsMeta: self isClassSide
		  selector: self selector
		  category: self protocolName
		  timeStamp: self stamp
		  source: self sourceCode
]
