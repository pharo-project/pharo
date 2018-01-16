Extension { #name : #RGMethodDefinition }

{ #category : #'*Ring-Monticello' }
RGMethodDefinition >> asMCMethodDefinition [
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

{ #category : #'*Ring-Monticello' }
RGMethodDefinition >> asMCMethodDefinitionFromActiveDefinition [
	"Creates a MCMethodDefinition from a source file assuming the current method isActive."

	| compiledMethod methodClass|
	compiledMethod := self compiledMethod.
	methodClass := compiledMethod methodClass.
	
   ^ MCMethodDefinition
		className: methodClass theNonMetaClass name
 	   	classIsMeta: methodClass isMeta
		selector: compiledMethod selector
		category: compiledMethod category
		timeStamp: compiledMethod timeStamp
		source: compiledMethod sourceCode
]

{ #category : #'*Ring-Monticello' }
RGMethodDefinition >> basicAsMCMethodDefinition [
   "Creates a MCMethodDefinition from the receiver"
   
	^ self isActive 
		ifTrue: [ self asMCMethodDefinitionFromActiveDefinition ]
		ifFalse: [
		   ^ MCMethodDefinition
				className: self theNonMetaParentName
		 	   	classIsMeta: self isMetaSide
				selector: self selector
				category: self protocol
				timeStamp: self stamp
				source: self sourceCode ]
]

{ #category : #'*Ring-Monticello' }
RGMethodDefinition >> sameAsMCDefinition: anMCMethodDefinition [
	^ anMCMethodDefinition selector = self selector
		and:
			[ 
			anMCMethodDefinition className = self className
				and:
					[ 
					anMCMethodDefinition classIsMeta = self isMetaSide
						and:
							[ 
							anMCMethodDefinition category = self protocol
								and: [ anMCMethodDefinition source = self sourceCode ] ] ] ]
]
