Class {
	#name : #PBSpurClassLoader,
	#superclass : #PBClassLoader,
	#instVars : [
		'nextClassIndex'
	],
	#category : 'Pharo30Bootstrap'
}

{ #category : #'as yet unclassified' }
PBSpurClassLoader >> classDefinitionFor: aClass [
	"Answer a String that defines the receiver."

	| type |
	type := aClass isWeak
						ifTrue: [ 'WeakLayout' ]
						ifFalse: [ aClass isPointers
									ifTrue: [ aClass isVariable
												ifTrue: [ 'VariableLayout' ]
												ifFalse: [ 'FixedLayout' ]
											]
									ifFalse: [  aClass isCompiledMethod
										ifTrue: [ 'CompiledMethodLayout' ]
										ifFalse: [ aClass isWords
											ifTrue: [ 'WordLayout' ]
											ifFalse: [ 'ByteLayout' ]]]].
	^ '| newClass |
	newClass := ShiftClassInstaller make: [ :builder |
		builder
			superclass: {superClass};
			name: ''{name}'';
			layoutClass: {type};
			slots: ''{instanceVariablesString}'' asSlotCollection;
			sharedVariablesFromString: ''{classVariablesString}'';
			sharedPools: ''{sharedPoolsString}'';
			category: ''{category}'';
			environment: {superClass} environment;
			classSlots: ''{classInstanceVariablesString}'' asSlotCollection ].
	"newClass setTraitComposition: {aTraitComposition} asTraitComposition."
	newClass'
		format: { 
			'superClass' -> (aClass superclass ifNil: ['nil'] ifNotNil: [ :superclass | superclass name ]).
			'name' -> aClass name.
			'type' -> type.
			'instanceVariablesString' -> (' ' join: aClass instVarNames).
			'classVariablesString' -> aClass classVariablesString.
			'sharedPoolsString' -> aClass sharedPoolsString.
			'category' -> aClass category asString.
			'classInstanceVariablesString' -> (' ' join: aClass classSide instVarNames).
			'aTraitComposition' -> aClass traitCompositionString } asDictionary.
]

{ #category : #'as yet unclassified' }
PBSpurClassLoader >> map: aClassMirror with: aDefinition [

	"We create a mapping between the real classes and their definitions for later"
	
	| classSideMirror |
	self assert: (aClassMirror target isKindOf: EPSimulatorHandle).
	(aClassMirror backend simulator objectMemory classTableRootObj) ifNotNil: [
		self assert: aClassMirror target asClassIndexHandle address notNil.
		aClassMirror target: (aClassMirror target asClassIndexHandle) ].

	mirrorDefinitionMap at: aClassMirror put: aDefinition.
	definitionMirrorMap at: aDefinition put: aClassMirror.

	classSideMirror := aClassMirror basicClass.
	"only valid for Spur simulator bootstrap by now"
	(aClassMirror backend simulator objectMemory classTableRootObj) ifNotNil: [
		self assert: classSideMirror target asClassIndexHandle address notNil.
		classSideMirror target: (classSideMirror target asClassIndexHandle) ].
	
	mirrorDefinitionMap at: classSideMirror put: aDefinition classSide.
	definitionMirrorMap at: aDefinition classSide put: classSideMirror.
]

{ #category : #'as yet unclassified' }
PBSpurClassLoader >> newClassIndex [
	
	nextClassIndex ifNil: [ nextClassIndex := 1024 ].
	^ nextClassIndex := nextClassIndex + 1
]

{ #category : #'as yet unclassified' }
PBSpurClassLoader >> registerClass: aClassMirror named: aName fullyLoaded: aBoolean [

	"If it existed before, we migrate its state"
	(environment includesKey: aName)
		ifTrue: [ 
			self migrateOldClass: (environment at: aName) to: aClassMirror ].

	"We register it and keep up to date our compact class index"
	environment at: aName put: aClassMirror.
	aClassMirror asClassMirror compactClassIndex = 0 ifFalse: [ 
		builder objectSpace backend compactClassAt: aClassMirror asClassMirror compactClassIndex put: aClassMirror ].
	
	"We create a mapping between the real classes and their definitions for later"
	self map: aClassMirror with: (systemDefinition classNamed: aName).
	
	"We say the simulator that this class is a root, possible becomes can come into it. THIS IS ONLY VALID FOR SPUR SIMULATOR BOOTSTRAP. REFACTOR ME".
	self flag: #refactorMe.
	builder objectSpace backend addExtraRoot: aClassMirror.
	builder objectSpace backend addExtraRoot: aClassMirror classSide.
	
	"We reset the class map of the ast interpreter"
	fullyLoaded at: aName put: aBoolean
]
