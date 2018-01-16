Extension { #name : #CompiledMethod }

{ #category : #'*Ring2-Compatibility' }
CompiledMethod >> asActiveRing2Definition [

	| env aMethod |
	
	env := RG2Environment new.
	aMethod := self ensureRing2DefinitionIn: env.
	env backend: (RG2ReadOnlyImageBackend for: env).
	^ aMethod
]

{ #category : #'*Ring2-Compatibility' }
CompiledMethod >> asRing2Definition [
	"Retrieves an active RG2MethodDefinition object based on the receiver.
	Note that its class is not converted."

	^ self ensureRing2DefinitionIn: RG2Environment new
]

{ #category : #'*Ring2-Compatibility' }
CompiledMethod >> asRing2DefinitionIn: aRG2Environment [

	^ (self methodClass asRing2DefinitionIn: aRG2Environment) methods detect: [:each | 
			each selector = self selector] ifNone: [ RG2NotFoundError signal ]
	
]

{ #category : #'*Ring2-Compatibility' }
CompiledMethod >> methodReference [

	| class selector |
	class := self methodClass ifNil: [^nil].
	selector := self selector ifNil: [^nil].
	^RG2MethodDefinition realClass: class selector: selector.
	
]
