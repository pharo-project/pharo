Extension { #name : #RG2MethodDefinition }

{ #category : #'*Ring2-Compatibility' }
RG2MethodDefinition >> <= aRG2MethodDefinition [ 
	
	self parent name < aRG2MethodDefinition parent name ifTrue: [^true].
	self parent name > aRG2MethodDefinition parent name ifTrue: [^false].
	
	^(self selector < aRG2MethodDefinition selector) 
 		  or: [ (self selector == aRG2MethodDefinition selector) and: [
    			  self time <= aRG2MethodDefinition time ]]
]

{ #category : #'*Ring2-Compatibility' }
RG2MethodDefinition >> actualClass [

	^ self realClass
]

{ #category : #'*Ring2-Compatibility' }
RG2MethodDefinition >> asActive [

	| aMethod |
	
	aMethod := self copy makeResolved.
	aMethod environment backend: RG2ReadOnlyImageBackend new. 
	^ aMethod. 
]

{ #category : #'*Ring2-Compatibility' }
RG2MethodDefinition >> asHistorical [

	^ self asStandalone
]

{ #category : #'*Ring2-Compatibility' }
RG2MethodDefinition >> asStandaloneWithPackage [

	| aDefinition |
	
	aDefinition := self copy.
	aDefinition environment backend: (RG2ReadOnlyImageBackend for: aDefinition environment). 
	aDefinition makeResolved.
	aDefinition environment backend: RG2EnvironmentBackend new. 
	^ aDefinition. 
]

{ #category : #'*Ring2-Compatibility' }
RG2MethodDefinition class >> className: aString selector: aString2 isMetaSide: isMeta [

	| aClass env |
	env := RG2Environment new.
	aClass := env ensureClassNamed: aString.
	^ aClass ensureLocalMethodNamed: aString2 asSymbol.	
		
		
]

{ #category : #'*Ring2-Compatibility' }
RG2MethodDefinition >> isDefined [
	"Answer whether the receiver exists in the environment"
	
	| rClass |
	self selector isDoIt ifTrue:[ ^false ].
	rClass := self realClass.
	^rClass notNil and: [ rClass includesSelector: self selector ]
]

{ #category : #'*Ring2-Compatibility' }
RG2MethodDefinition >> isLocalSelector [
	"Answer whether the receiver exists in the environment as a local implementation"
	
	| rClass |
	rClass:= self realClass.
	^rClass notNil and: [ rClass includesLocalSelector: self selector ]
]

{ #category : #'*Ring2-Compatibility' }
RG2MethodDefinition >> literals [
	^ self method literals
]

{ #category : #'*Ring2-Compatibility' }
RG2MethodDefinition >> method [

	| aSelector aClass |
	aClass := (self class environment classNamed: self parent name).
	aClass ifNil: [ ^ nil ].
	aSelector := self name asSymbol.
	^ (aClass includesSelector: aSelector)
		ifTrue: [ aClass>>aSelector ]
		ifFalse: [ nil ]
]

{ #category : #'*Ring2-Compatibility' }
RG2MethodDefinition >> realClass [

	^ self parent realClass
]

{ #category : #'*Ring2-Compatibility' }
RG2MethodDefinition class >> realClass: aClass selector: aString [

	| env aMethod |
	
	env := RG2Environment new.
	aMethod := (aClass>>aString asSymbol) ensureRing2DefinitionIn: env.
	env backend: (RG2ReadOnlyImageBackend for: env).
	^ aMethod
]

{ #category : #'*Ring2-Compatibility' }
RG2MethodDefinition >> sourcePointer [
	
	^ self method sourcePointer
	"
	^ self isActive 
		ifTrue: [ self compiledMethod sourcePointer ]
		ifFalse: [ self annotationNamed: self class sourcePointerKey ]		
		"
]

{ #category : #'*Ring2-Compatibility' }
RG2MethodDefinition >> stamp [

	^ '<historical>'
]

{ #category : #'*Ring2-Compatibility' }
RG2MethodDefinition >> theNonMetaClassName [

	^ self parent instanceSide name
]
