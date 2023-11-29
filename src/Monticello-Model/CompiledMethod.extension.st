Extension { #name : 'CompiledMethod' }

{ #category : '*Monticello-Model' }
CompiledMethod >> asMCMethodDefinition [
	"Creates a MCMethodDefinition from the receiver"

	| cached |
	cached := MCMethodDefinition cachedDefinitions at: self ifAbsent: [ nil ].

	"we compare that the cached version is in sync with the version 
	the receiver represents because it is an identity structure and the container (here the method definition may have changed internally: different packages, protocol.... )"
	(cached notNil and: [ self sameAsMCDefinition: cached ]) ifFalse: [
		cached := self basicAsMCMethodDefinition.
		MCMethodDefinition cachedDefinitions at: self put: cached ].

	^ cached
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

{ #category : '*Monticello-Model' }
CompiledMethod >> sameAsMCDefinition: anMCMethodDefinition [

	^ anMCMethodDefinition selector = self selector and: [
		  anMCMethodDefinition className = self className and: [
			  anMCMethodDefinition classIsMeta = self isClassSide and: [
				  anMCMethodDefinition protocol = self protocolName and: [ anMCMethodDefinition source = self sourceCode ] ] ] ]
]
