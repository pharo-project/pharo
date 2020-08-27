Extension { #name : #CompiledMethod }

{ #category : #'*Monticello' }
CompiledMethod >> asMCMethodDefinition [
   "Creates a MCMethodDefinition from the receiver"
	| cached |
	cached := MCMethodDefinition cachedDefinitions
		at: self compiledMethod
		ifAbsent: [ nil ].

	"we compare that the cached version is in sync with the version 
	the receiver represents because it is an identity structure and the container (here the method definition may have changed internally: different packages, protocol.... )"
	(cached notNil and: [ self sameAsMCDefinition: cached ]) ifFalse: [
		cached := self basicAsMCMethodDefinition.
		MCMethodDefinition cachedDefinitions 
			at: self compiledMethod 
			put: cached ].

	^ cached
]

{ #category : #'*Monticello' }
CompiledMethod >> basicAsMCMethodDefinition [
   "Creates a MCMethodDefinition from the receiver"
   
		   ^ MCMethodDefinition
				className: self methodClass instanceSide name
		 	   	classIsMeta: self methodClass isClassSide
				selector: self selector
				category: self protocol
				timeStamp: self stamp
				source: self sourceCode 
]

{ #category : #'*Monticello' }
CompiledMethod >> sameAsMCDefinition: anMCMethodDefinition [
	^ anMCMethodDefinition selector = self selector
		and:
			[ 
			anMCMethodDefinition className = self className
				and:
					[ 
					anMCMethodDefinition classIsMeta = self methodClass isClassSide
						and:
							[ 
							anMCMethodDefinition category = self protocol
								and: [ anMCMethodDefinition source = self sourceCode ] ] ] ]
]
