Class {
	#name : #RG2ReadOnlyImageBackendTest,
	#superclass : #RG2BackendTest,
	#traits : 'TRG2ReadOnlyDefinitionTest',
	#classTraits : 'TRG2ReadOnlyDefinitionTest classTrait',
	#category : #'Ring2-Tests-Core'
}

{ #category : #tests }
RG2ReadOnlyImageBackendTest >> backendClass [

	^ RG2ReadOnlyImageBackend 
]

{ #category : #accessing }
RG2ReadOnlyImageBackendTest >> newEnvironment [

	| env | 
	
	env := RG2Environment unnamed.
	env backend: (RG2ReadOnlyImageBackend for: env).
	
	^ env
]

{ #category : #tests }
RG2ReadOnlyImageBackendTest >> testBehavior [

	| env1 point point2 object method |
	
	env1 := RG2Environment new.
	env1 backend: (RG2ReadOnlyImageBackend for: env1).
	
	point := Point asRing2MinimalDefinitionIn: env1.
	"different access method"
	point2 := env1 ask behaviors detect: [ :each | each name == #Point ].
	object := Object asRing2MinimalDefinitionIn: env1.
	method := (Point>>#x) asRing2MinimalDefinitionIn: env1.
		
	self assert: (point == point2).
	self assert: (point superclass == object).
	self assert: (point == method parent).
	
	self assert: (point unresolvedProperties size) equals: 0.
	self assert: point ask localMethods size equals: Point localMethods size.
	
	
]

{ #category : #tests }
RG2ReadOnlyImageBackendTest >> testBehaviorLocalMethods [

	| env1 point selectors |
	
	env1 := RG2Environment new.
	env1 backend: (RG2ReadOnlyImageBackend for: env1).

	point := Point asRing2MinimalDefinitionIn: env1.
	
	self assert: point ask localMethods size equals: Point localMethods size.
	self assert: (point ask localMethods 
		allSatisfy: [:each | each isKindOf: RG2MethodDefinition]).
	selectors := point ask localMethods collect: [ :each | each ask selector ].
	self assert: (selectors allSatisfy: #isSymbol).
	self assert: selectors asSortedCollection equals: Point localSelectors asSortedCollection.
	
	
]

{ #category : #tests }
RG2ReadOnlyImageBackendTest >> testClassComment [

	| env comment |
	
	env := RG2Environment new.
	env backend: (RG2ReadOnlyImageBackend for: env).
	
	comment := (Point asRing2MinimalDefinitionIn: env) comment.
	
	comment content notEmpty.
	comment author notEmpty.
	comment time > DateAndTime new.
	comment time <= DateAndTime now.
	
	
]

{ #category : #tests }
RG2ReadOnlyImageBackendTest >> testClassTrait [

	| env classTrait |
	
	env := RG2Environment new.
	env backend: (RG2ReadOnlyImageBackend for: env).
	
	classTrait := RG2TestTrait classTrait asRing2MinimalDefinitionIn: env.
	self assert: classTrait isTrait.
	self assert: classTrait isMeta.
	self assert: classTrait name equals: 'RG2TestTrait classTrait'.
	self assert: classTrait baseTrait name equals: 'RG2TestTrait'.
	self assert: classTrait category equals: 'Ring2-Tests-Support'.
	self assert: (classTrait superclass == classTrait). "cycle, nil in reality"
	self assert: classTrait comment content equals: RG2TestTrait organization comment.
	self assert: classTrait localMethods size equals: RG2TestTrait classTrait localMethods size.
	self assert: classTrait protocols asSortedCollection equals: RG2TestTrait classTrait protocols asSortedCollection.
	self assert: classTrait metaclass name equals: #ClassTrait.
	self assert: classTrait package name equals: 'Ring2-Tests-Support'.
	

]

{ #category : #tests }
RG2ReadOnlyImageBackendTest >> testClassVariable [

	| env classVariable |
	
	env := RG2Environment new.
	env backend: (RG2ReadOnlyImageBackend for: env).
	
	classVariable := (UIManager classVariableNamed: #Default) asRing2MinimalDefinitionIn: env.
	self assert: classVariable isClassVariable.
	self assert: classVariable name equals: #Default.
	self assert: classVariable parent name = #UIManager.

	

]

{ #category : #accessing }
RG2ReadOnlyImageBackendTest >> testDefaultEnvironmentContent [

	| env |
	
	env := RG2Environment unnamed.
	env backend: (RG2ReadOnlyImageBackend for: env).
	
	self testDefaultContentFor: env.
	
			
	
	

]

{ #category : #tests }
RG2ReadOnlyImageBackendTest >> testDefinitionsIdentity [

	| env1 |
	
	env1 := RG2Environment new.
	env1 backend: (RG2ReadOnlyImageBackend for: env1).
	
	self assert: (((Object>>#isRing2Object) asRing2MinimalDefinitionIn: env1) == ((Object>>#isRing2Object) asRing2MinimalDefinitionIn: env1)).
	
	self assert: (((Object>>#isRing2Object) asRing2MinimalDefinitionIn: env1) parent
== ((Object>>#isRing2Object) asRing2MinimalDefinitionIn: env1) parent).
]

{ #category : #tests }
RG2ReadOnlyImageBackendTest >> testEmptyLayout [

	| env layout |
	
	env := RG2Environment new.
	env backend: (RG2ReadOnlyImageBackend for: env).
	
	self should: [EmptyLayout instance asRing2MinimalDefinitionIn: env] raise: Error.
]

{ #category : #tests }
RG2ReadOnlyImageBackendTest >> testGlobalVariable [

	| env real globalVariable |
	
	env := RG2Environment new.
	env backend: (RG2ReadOnlyImageBackend for: env).
	
	real := (Smalltalk globals associations detect: [ :each | each key = #Smalltalk ]).
	globalVariable := real asRing2MinimalDefinitionIn: env.
	
	self assert: (globalVariable name = #Smalltalk)
]

{ #category : #tests }
RG2ReadOnlyImageBackendTest >> testLayouts [

	| env layout |
	
	env := RG2Environment new.
	env backend: (RG2ReadOnlyImageBackend for: env).
	
	layout := (ByteString asRing2MinimalDefinitionIn: env) layout.
	self assert: layout isLayout.
	self assert: layout isByteLayout.

	layout := (WordArray asRing2MinimalDefinitionIn: env) layout.
	self assert: layout isLayout.
	self assert: layout isWordLayout.	

	layout := (CompiledMethod asRing2MinimalDefinitionIn: env) layout.
	self assert: layout isLayout.
	self assert: layout isCompiledMethodLayout.	

	layout := (CompiledMethod asRing2MinimalDefinitionIn: env) layout.
	self assert: layout isLayout.
	self assert: layout isCompiledMethodLayout.				
		
	layout := (SmallInteger asRing2MinimalDefinitionIn: env) layout.
	self assert: layout isLayout.
	self assert: layout isImmediateLayout.				
		
	layout := (Ephemeron asRing2MinimalDefinitionIn: env) layout.
	self assert: layout isLayout.
	self assert: layout isEphemeronLayout.				

	layout := (Object asRing2MinimalDefinitionIn: env) layout.
	self assert: layout isLayout.
	self assert: layout isFixedLayout.				

	layout := (Array asRing2MinimalDefinitionIn: env) layout.
	self assert: layout isLayout.
	self assert: layout isVariableLayout.				

	layout := (WeakArray asRing2MinimalDefinitionIn: env) layout.
	self assert: layout isLayout.
	self assert: layout isWeakLayout.				

			
]

{ #category : #tests }
RG2ReadOnlyImageBackendTest >> testMethod [

	| env method |
	
	env := RG2Environment new.
	env backend: (RG2ReadOnlyImageBackend for: env).
	
	method := (Point>>#x) asRing2MinimalDefinitionIn: env.
	
	method author notEmpty.
	method time > DateAndTime new.
	method time <= DateAndTime now.
	
	self assert: method selector equals: #x.
	self assert: (method package isRing2Object and: [method package isPackage]).
	self assert: method package name equals: 'Kernel'.
	self assert: (method package == method parent package).
	self assert: method sourceCode equals: (Point>>#x) sourceCode.	
	self assert: method ast equals: (Point>>#x) ast.
		
]

{ #category : #tests }
RG2ReadOnlyImageBackendTest >> testProtocols [

	| env1 point method method2 protocol protocol2 |
	
	env1 := RG2Environment new.
	env1 backend: (RG2ReadOnlyImageBackend for: env1).
	
	point := Point asRing2MinimalDefinitionIn: env1.
	self assert: (point protocols notEmpty).
	protocol := point protocols detect: [ :each | each = 'accessing' ].
	self assert: (protocol isSymbol).
	
	method := (Point>>#x) asRing2MinimalDefinitionIn: env1.
	protocol := method protocol.
	method2 := (Point>>#y) asRing2MinimalDefinitionIn: env1.
	protocol2 := method2 protocol.
	
	self assert: (protocol == protocol2).
	self assert: (protocol isSymbol).
	self assert: protocol equals: 'accessing'.
	
	self should: [ (Protocol name: 'aProtocol') asRing2MinimalDefinitionIn: RG2Environment new ] raise: Error.
]

{ #category : #tests }
RG2ReadOnlyImageBackendTest >> testSlots [

	| env slot |
	
	env := RG2Environment new.
	env backend: (RG2ReadOnlyImageBackend for: env).
	
	slot := (Point slotNamed: #x) asRing2MinimalDefinitionIn: env.
	self assert: slot isSlot.
	self assert: slot name equals: #x.
	self assert: slot parent isLayout.
	self assert: slot parent isFixedLayout.
	self assert: slot parent parent name equals: #Point.
	

]

{ #category : #tests }
RG2ReadOnlyImageBackendTest >> testTrait [

	| env trait |
	
	env := RG2Environment new.
	env backend: (RG2ReadOnlyImageBackend for: env).
	
	trait := RG2TestTrait asRing2MinimalDefinitionIn: env.
	self assert: trait isTrait.
	self assert: trait isMeta not.
	self assert: trait name equals: #RG2TestTrait.
	self assert: trait classTrait name equals: 'RG2TestTrait classTrait'.
	self assert: trait category equals: 'Ring2-Tests-Support'.
	self assert: (trait superclass == trait). "cycle, nil in reality"
	self assert: trait comment content equals: RG2TestTrait organization comment.
	self assert: trait localMethods size equals: RG2TestTrait localMethods size.
	self assert: trait protocols asSortedCollection equals: RG2TestTrait protocols asSortedCollection.
	self assert: trait metaclass name equals: #Trait.
	self assert: trait package name equals: 'Ring2-Tests-Support'.
	

]

{ #category : #tests }
RG2ReadOnlyImageBackendTest >> testTraitAlias [

	| env traitAlias |
	
	env := RG2Environment new.
	env backend: (RG2ReadOnlyImageBackend for: env).
	
	traitAlias := (MOPTestClassD traitComposition transformations first) asRing2MinimalDefinitionIn: env.
	self assert: traitAlias isTraitAlias.
	self assert: traitAlias subject name equals: #Trait2.
	self assert: traitAlias aliases size equals: 1.
	self assert: traitAlias aliases first key == #c3.
	self assert: traitAlias aliases first value == #c2.	
	
	
	
]

{ #category : #tests }
RG2ReadOnlyImageBackendTest >> testTraitComposition [

	| env traitComposition |
	
	env := RG2Environment new.
	env backend: (RG2ReadOnlyImageBackend for: env).
	
	traitComposition := RG2TestClass traitComposition asRing2MinimalDefinitionIn: env.
	self assert: traitComposition isTraitComposition.
	self assert: traitComposition transformations size equals: 1.
	self assert: traitComposition transformations first name equals: #RG2TestTrait.
	self assert: traitComposition transformations first isTrait.
	self assert: traitComposition transformations first isRing2Object.
	
	self assert: traitComposition parent traitComposition equals: traitComposition.
	
]

{ #category : #tests }
RG2ReadOnlyImageBackendTest >> testTraitExclusions [

	| env traitExclusion |
	
	env := RG2Environment new.
	env backend: (RG2ReadOnlyImageBackend for: env).
	
	traitExclusion := (MOPTestClassB traitComposition transformations second) asRing2MinimalDefinitionIn: env.
	self assert: traitExclusion isTraitExclusion.
	self assert: traitExclusion subject name equals: #Trait2.
	self assert: traitExclusion exclusions size equals: 1.
	self assert: traitExclusion exclusions first equals: #c.

	
	
	
]

{ #category : #tests }
RG2ReadOnlyImageBackendTest >> testUnknownSlots [

	| env slot |
	
	env := RG2Environment new.
	env backend: (RG2ReadOnlyImageBackend for: env).
	
	slot := (SlotExamplePerson slotNamed: #directedMovies) asRing2MinimalDefinitionIn: env.
	self assert: slot isSlot.
	self assert: slot name equals: #directedMovies.
	self assert: slot expression equals: 'ToManyRelationSlot inverse: #director inClass: #SlotExampleMovie'.
	self assert: slot parent isLayout.
	self assert: slot parent isFixedLayout.
	self assert: slot parent parent name equals: #SlotExamplePerson.
	
	

]
