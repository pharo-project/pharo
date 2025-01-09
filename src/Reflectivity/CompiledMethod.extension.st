Extension { #name : 'CompiledMethod' }

{ #category : '*Reflectivity' }
CompiledMethod >> assignmentNodes [
	^self ast assignmentNodes
]

{ #category : '*Reflectivity' }
CompiledMethod >> blockNodes [
	^self ast blockNodes
]

{ #category : '*Reflectivity' }
CompiledMethod >> compiledMethod [
	^self
]

{ #category : '*Reflectivity' }
CompiledMethod >> createTwin [
	self reflectiveMethod: (ReflectiveMethod on: self)
]

{ #category : '*Reflectivity' }
CompiledMethod >> destroyTwin [
	self reflectiveMethod ifNotNil: [:rf| rf destroyTwin ]
]

{ #category : '*Reflectivity' }
CompiledMethod >> hasBreakpoint [
	^ Breakpoint isInstalledIn: self
]

{ #category : '*Reflectivity' }
CompiledMethod >> hasMetaLinks [
	self reflectiveMethod ifNil: [ ^false ].
	^self reflectiveMethod hasMetaLinks
]

{ #category : '*Reflectivity' }
CompiledMethod >> installLink: aMetaLink [
	self reflectiveMethod ifNil: [ self createTwin ].
	self reflectiveMethod increaseLinkCount.
	(aMetaLink optionCompileOnLinkInstallation or: [ self isRealPrimitive ])
		ifTrue: [ self reflectiveMethod compileAndInstallCompiledMethod ]
		ifFalse: [ self invalidate ]
]

{ #category : '*Reflectivity' }
CompiledMethod >> instanceVariableReadNodes [
	^self ast instanceVariableReadNodes
]

{ #category : '*Reflectivity' }
CompiledMethod >> instanceVariableWriteNodes [
	^self ast instanceVariableWriteNodes
]

{ #category : '*Reflectivity' }
CompiledMethod >> invalidate [

	| reflectiveMethod |
	self reflectivityDisabled ifTrue: [ ^ self ].

	reflectiveMethod := self reflectiveMethod.
	reflectiveMethod ifNil: [ ^ self "do nothing" ].
	(self isRealPrimitive or:
		 (reflectiveMethod ast metaLinkOptionsFromClassAndMethod
			  includesOption: #optionCompileOnLinkInstallation))
		ifTrue: [ reflectiveMethod compileAndInstallCompiledMethod ]
		ifFalse: [ reflectiveMethod installReflectiveMethod ]
]

{ #category : '*Reflectivity' }
CompiledMethod >> link: aMetaLink toVariableNamed: vName option: accessStrategy [
	^aMetaLink
		installOnVariable: (self lookupVar: vName)
		for: self methodClass
		option: accessStrategy
		instanceSpecific: false
]

{ #category : '*Reflectivity' }
CompiledMethod >> metaLinkOptions [
	^{
	#penultimateLiteral -> #( + optionCompileOnLinkInstallation).
	#selector -> #( + optionCompileOnLinkInstallation).
	#objectAt: -> #( + optionCompileOnLinkInstallation).
	#header -> #( + optionCompileOnLinkInstallation).
	#numLiterals -> #( + optionCompileOnLinkInstallation).
	#literalAt: -> #( + optionCompileOnLinkInstallation).
	#metaLinkOptions -> #( + optionCompileOnLinkInstallation)
	}
]

{ #category : '*Reflectivity' }
CompiledMethod >> newBreakpointForVariable: aVariableName [
	^ (self tempNames includes: aVariableName)
		ifTrue: [ VariableBreakpoint
				newBreakpointForVariable: aVariableName
				forMethod: self ]
		ifFalse: [ VariableBreakpoint
				newBreakpointForVariable: aVariableName
				inClass: self methodClass ]
]

{ #category : '*Reflectivity' }
CompiledMethod >> reflectiveMethod [
	^self propertyAt: #reflectiveMethod ifAbsent: nil
]

{ #category : '*Reflectivity' }
CompiledMethod >> reflectiveMethod: aReflectiveMethod [
	self propertyAt: #reflectiveMethod put: aReflectiveMethod
]

{ #category : '*Reflectivity' }
CompiledMethod >> reflectivityDisabled [
	^self hasPragmaNamed: #disableReflectivity
]

{ #category : '*Reflectivity' }
CompiledMethod >> removeLink: aLink [

	self reflectiveMethod ifNotNil: [:rm | rm removeLink: aLink]
]

{ #category : '*Reflectivity' }
CompiledMethod >> sendNodes [
	^self ast sendNodes
]

{ #category : '*Reflectivity' }
CompiledMethod >> variableNodes [
	^self ast variableNodes
]

{ #category : '*Reflectivity' }
CompiledMethod >> variableReadNodes [
	^self ast variableReadNodes
]

{ #category : '*Reflectivity' }
CompiledMethod >> variableWriteNodes [
	^self ast variableWriteNodes
]
