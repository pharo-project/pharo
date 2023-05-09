Class {
	#name : #RGReadOnlyImageBackendTest,
	#superclass : #RGBackendTest,
	#traits : 'TRGReadOnlyTest',
	#classTraits : 'TRGReadOnlyTest classTrait',
	#category : #'Ring-Tests-Core'
}

{ #category : #tests }
RGReadOnlyImageBackendTest >> backendClass [

	^ RGReadOnlyImageBackend
]

{ #category : #accessing }
RGReadOnlyImageBackendTest >> newEnvironment [

	| env |

	env := RGEnvironment unnamed.
	env backend: (RGReadOnlyImageBackend for: env).

	^ env
]

{ #category : #tests }
RGReadOnlyImageBackendTest >> testBehavior [
	| env1 point point2 object method |
	env1 := RGEnvironment new.
	env1 backend: (RGReadOnlyImageBackend for: env1).

	point := Point asRingMinimalDefinitionIn: env1.
	"different access method"
	point2 := env1 ask behaviors detect: [ :each | each name == #Point ].
	object := Object asRingMinimalDefinitionIn: env1.
	method := Point >> #x asRingMinimalDefinitionIn: env1.

	self assert: point identicalTo: point2.
	self assert: point superclass identicalTo: object.
	self assert: point identicalTo: method parent.

	self assert: point unresolvedProperties size equals: 0.
	self assert: point ask localMethods size equals: Point localMethods size
]

{ #category : #tests }
RGReadOnlyImageBackendTest >> testBehaviorLocalMethods [

	| env1 point selectors |

	env1 := RGEnvironment new.
	env1 backend: (RGReadOnlyImageBackend for: env1).

	point := Point asRingMinimalDefinitionIn: env1.

	self assert: point ask localMethods size equals: Point localMethods size.
	self assert: (point ask localMethods
		allSatisfy: [:each | each isKindOf: RGMethod]).
	selectors := point ask localMethods collect: [ :each | each ask selector ].
	self assert: (selectors allSatisfy: [:each | each isSymbol]).
	self assert: selectors asSortedCollection equals: Point localSelectors asSortedCollection
]

{ #category : #tests }
RGReadOnlyImageBackendTest >> testClassComment [

	| env comment |

	env := RGEnvironment new.
	env backend: (RGReadOnlyImageBackend for: env).

	comment := (Point asRingMinimalDefinitionIn: env) comment.

	comment content notEmpty.
	comment author notEmpty.
	comment time > DateAndTime new.
	comment time <= DateAndTime now
]

{ #category : #tests }
RGReadOnlyImageBackendTest >> testClassTrait [

	| env classTrait |

	env := RGEnvironment new.
	env backend: (RGReadOnlyImageBackend for: env).

	classTrait := RGTestTrait classTrait asRingMinimalDefinitionIn: env.
	self assert: classTrait isTrait.
	self assert: classTrait isMeta.
	self assert: classTrait name equals: 'RGTestTrait classTrait'.
	self assert: classTrait baseTrait name equals: 'RGTestTrait'.
	self assert: classTrait category equals: 'Ring-Tests-Core'.
	self assert: classTrait superclass name equals: #Trait.
	self assert: classTrait comment content equals: RGTestTrait comment.
	self assert: classTrait localMethods size equals: RGTestTrait classTrait localMethods size.
	self assert: classTrait protocols asSortedCollection equals: RGTestTrait classTrait protocolNames asSortedCollection.
	self assert: classTrait metaclass name equals: #MetaclassForTraits.
	self assert: classTrait package name equals: 'Ring-Tests-Core'
]

{ #category : #tests }
RGReadOnlyImageBackendTest >> testClassVariable [
	| env classVariable |
	env := RGEnvironment new.
	env backend: (RGReadOnlyImageBackend for: env).

	classVariable := (UIManager classVariableNamed: #Default) asRingMinimalDefinitionIn: env.
	self assert: classVariable isClassVariable.
	self assert: classVariable name equals: #Default.
	self assert: classVariable parent name equals: #UIManager
]

{ #category : #accessing }
RGReadOnlyImageBackendTest >> testDefaultEnvironmentContent [

	| env |

	env := RGEnvironment unnamed.
	env backend: (RGReadOnlyImageBackend for: env).

	self testDefaultContentFor: env
]

{ #category : #tests }
RGReadOnlyImageBackendTest >> testDefinitionsIdentity [
	| env1 |
	env1 := RGEnvironment new.
	env1 backend: (RGReadOnlyImageBackend for: env1).

	self assert: (Object >> #isRGObject asRingMinimalDefinitionIn: env1) identicalTo: (Object >> #isRGObject asRingMinimalDefinitionIn: env1).

	self assert: (Object >> #isRGObject asRingMinimalDefinitionIn: env1) parent identicalTo: (Object >> #isRGObject asRingMinimalDefinitionIn: env1) parent
]

{ #category : #tests }
RGReadOnlyImageBackendTest >> testEmptyLayout [

	| env |

	env := RGEnvironment new.
	env backend: (RGReadOnlyImageBackend for: env).

	self should: [EmptyLayout instance asRingMinimalDefinitionIn: env] raise: Error
]

{ #category : #tests }
RGReadOnlyImageBackendTest >> testGlobalVariable [
	| env real globalVariable |
	env := RGEnvironment new.
	env backend: (RGReadOnlyImageBackend for: env).

	real := Smalltalk globals associations detect: [ :each | each key = #Smalltalk ].
	globalVariable := real asRingMinimalDefinitionIn: env.

	self assert: globalVariable name equals: #Smalltalk
]

{ #category : #tests }
RGReadOnlyImageBackendTest >> testLayouts [

	| env layout |

	env := RGEnvironment new.
	env backend: (RGReadOnlyImageBackend for: env).

	layout := (ByteString asRingMinimalDefinitionIn: env) layout.
	self assert: layout isLayout.
	self assert: layout isByteLayout.

	layout := (WordArray asRingMinimalDefinitionIn: env) layout.
	self assert: layout isLayout.
	self assert: layout isWordLayout.

	layout := (CompiledMethod asRingMinimalDefinitionIn: env) layout.
	self assert: layout isLayout.
	self assert: layout isCompiledMethodLayout.

	layout := (CompiledMethod asRingMinimalDefinitionIn: env) layout.
	self assert: layout isLayout.
	self assert: layout isCompiledMethodLayout.

	layout := (SmallInteger asRingMinimalDefinitionIn: env) layout.
	self assert: layout isLayout.
	self assert: layout isImmediateLayout.

	layout := (FinalizationRegistryEntry asRingMinimalDefinitionIn: env) layout.
	self assert: layout isLayout.
	self assert: layout isEphemeronLayout.

	layout := (Object asRingMinimalDefinitionIn: env) layout.
	self assert: layout isLayout.
	self assert: layout isFixedLayout.

	layout := (Array asRingMinimalDefinitionIn: env) layout.
	self assert: layout isLayout.
	self assert: layout isVariableLayout.

	layout := (WeakArray asRingMinimalDefinitionIn: env) layout.
	self assert: layout isLayout.
	self assert: layout isWeakLayout
]

{ #category : #tests }
RGReadOnlyImageBackendTest >> testMethod [
	| env method |
	env := RGEnvironment new.
	env backend: (RGReadOnlyImageBackend for: env).

	method := Point >> #x asRingMinimalDefinitionIn: env.

	self assert: method author notEmpty.
	self assert: (method time <= DateAndTime now).
	self assert: method selector equals: #x.
	self assert: (method package isRGObject and: [ method package isPackage ]).
	self assert: method package name equals: 'Kernel'.
	self assert: method package identicalTo: method parent package.
	self assert: method sourceCode equals: (Point >> #x) sourceCode.
	self assert: method ast equals: (Point >> #x) ast
]

{ #category : #tests }
RGReadOnlyImageBackendTest >> testProtocols [
	| env1 point method method2 protocol protocol2 |
	env1 := RGEnvironment new.
	env1 backend: (RGReadOnlyImageBackend for: env1).

	point := Point asRingMinimalDefinitionIn: env1.
	self assert: point protocols notEmpty.
	protocol := point protocols detect: [ :each | each = 'accessing' ].
	self assert: protocol isSymbol.

	method := Point >> #x asRingMinimalDefinitionIn: env1.
	protocol := method protocol.
	method2 := Point >> #y asRingMinimalDefinitionIn: env1.
	protocol2 := method2 protocol.

	self assert: protocol identicalTo: protocol2.
	self assert: protocol isSymbol.
	self assert: protocol equals: 'accessing'.

	self should: [ (Protocol name: 'aProtocol') asRingMinimalDefinitionIn: RGEnvironment new ] raise: Error
]

{ #category : #tests }
RGReadOnlyImageBackendTest >> testSlots [

	| env slot |

	env := RGEnvironment new.
	env backend: (RGReadOnlyImageBackend for: env).

	slot := (Point slotNamed: #x) asRingMinimalDefinitionIn: env.
	self assert: slot isSlot.
	self assert: slot name equals: #x.
	self assert: slot parent isLayout.
	self assert: slot parent isFixedLayout.
	self assert: slot parent parent name equals: #Point
]

{ #category : #tests }
RGReadOnlyImageBackendTest >> testTrait [
	| env trait |
	env := RGEnvironment new.
	env backend: (RGReadOnlyImageBackend for: env).

	trait := RGTestTrait asRingMinimalDefinitionIn: env.
	self assert: trait isTrait.
	self assert: trait isMeta not.
	self assert: trait name equals: #RGTestTrait.
	self assert: trait classTrait name equals: 'RGTestTrait classTrait'.
	self assert: trait category equals: 'Ring-Tests-Core'.
	self assert: trait superclass identicalTo: trait.	"cycle, nil in reality"
	self assert: trait comment content equals: RGTestTrait comment.
	self assert: trait localMethods size equals: RGTestTrait localMethods size.
	self assert: trait protocols asSortedCollection equals: RGTestTrait protocolNames asSortedCollection.
	self assert: trait metaclass name equals: 'RGTestTrait classTrait'.
	self assert: trait metaclass superclass name equals: 'Trait'.
	self assert: trait package name equals: 'Ring-Tests-Core'
]

{ #category : #tests }
RGReadOnlyImageBackendTest >> testTraitAlias [
	| env traitAlias |
	env := RGEnvironment new.
	env backend: (RGReadOnlyImageBackend for: env).

	traitAlias := MOPTestClassD traitComposition transformations first asRingMinimalDefinitionIn: env.
	self assert: traitAlias isTraitAlias.
	self assert: traitAlias subject name equals: #Trait2.
	self assert: traitAlias aliases size equals: 1.
	self assert: traitAlias aliases first key identicalTo: #c3.
	self assert: traitAlias aliases first value identicalTo: #c2
]

{ #category : #tests }
RGReadOnlyImageBackendTest >> testTraitComposition [

	| env traitComposition |

	env := RGEnvironment new.
	env backend: (RGReadOnlyImageBackend for: env).

	traitComposition := RGTestClass traitComposition asRingMinimalDefinitionIn: env.
	self assert: traitComposition isTraitComposition.
	self assert: traitComposition transformations size equals: 1.
	self assert: traitComposition transformations first name equals: #RGTestTrait.
	self assert: traitComposition transformations first isTrait.
	self assert: traitComposition transformations first isRGObject.

	self assert: traitComposition parent traitComposition equals: traitComposition
]

{ #category : #tests }
RGReadOnlyImageBackendTest >> testTraitExclusions [

	| env traitExclusion |

	env := RGEnvironment new.
	env backend: (RGReadOnlyImageBackend for: env).

	traitExclusion := (MOPTestClassB traitComposition transformations second) asRingMinimalDefinitionIn: env.
	self assert: traitExclusion isTraitExclusion.
	self assert: traitExclusion subject name equals: #Trait2.
	self assert: traitExclusion exclusions size equals: 1.
	self assert: traitExclusion exclusions first equals: #c
]

{ #category : #tests }
RGReadOnlyImageBackendTest >> testUnknownSlots [

	| env slot |

	env := RGEnvironment new.
	env backend: (RGReadOnlyImageBackend for: env).

	slot := (SlotExamplePerson slotNamed: #directedMovies) asRingMinimalDefinitionIn: env.
	self assert: slot isSlot.
	self assert: slot name equals: #directedMovies.
	self assert: slot expression equals: 'ToManyRelationSlot inverse: #director inClass: #SlotExampleMovie'.
	self assert: slot parent isLayout.
	self assert: slot parent isFixedLayout.
	self assert: slot parent parent name equals: #SlotExamplePerson
]
