Class {
	#name : 'PBClassLoader',
	#superclass : 'Object',
	#instVars : [
		'nextClassIndex',
		'builder',
		'useClassBuilder',
		'initializedClasses',
		'compactClasses',
		'mirrorDefinitionMap',
		'definitionMirrorMap',
		'shouldInitializeClassPool',
		'environment',
		'systemDefinition',
		'fullyLoaded'
	],
	#category : 'PharoBootstrap',
	#package : 'PharoBootstrap'
}

{ #category : 'accessing' }
PBClassLoader >> builder: aBuilder [

	builder := aBuilder
]

{ #category : 'accessing' }
PBClassLoader >> classDefinitionFor: aClass [
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
			name: {name};
			layoutClass: {type};
			slots: {instanceVariablesString} asSlotCollection;
			sharedVariablesFromString: {classVariablesString};
			sharedPools: {sharedPoolsString};
			package: {package};
			tag: {tag};
			environment: {superClass} environment;
			classSlots: {classInstanceVariablesString} asSlotCollection ].
	"newClass setTraitComposition: {aTraitComposition} asTraitComposition."
	newClass'
		format: { 
			'superClass' -> (aClass superclass ifNil: ['nil'] ifNotNil: [ :superclass | superclass name ]).
			'name' -> aClass name printString.
			'type' -> type.
			'instanceVariablesString' -> (' ' join: aClass instVarNames) printString.
			'classVariablesString' -> aClass classVariablesString printString.
			'sharedPoolsString' -> aClass sharedPoolsString printString.
			'package' -> aClass package name printString.
			'tag' -> (aClass tags ifEmpty: [ 'nil' ] ifNotEmpty: [ :tags | tags anyOne printString ]).
			'classInstanceVariablesString' -> (' ' join: aClass classSide instVarNames) printString.
			'aTraitComposition' -> aClass traitCompositionString } asDictionary.
]

{ #category : 'accessing' }
PBClassLoader >> classForClassMirror: anEPClassMirror [ 
	
	^ mirrorDefinitionMap at: anEPClassMirror ifAbsent: [ systemDefinition classNamed: #Class ]
]

{ #category : 'class-loading' }
PBClassLoader >> classNamed: aString [ 
	
	| class |
	(self loadedClassNamed: aString)
		ifTrue: [ ^ environment at: aString ].
	class := self loadClassNamed: aString.
	^ class
]

{ #category : 'stubs' }
PBClassLoader >> compactClassIndexForClass: aRFiClass [ 
	
	^ builder compactClassIndexForClassNamed: aRFiClass name.
]

{ #category : 'class-loading' }
PBClassLoader >> createBehaviorFromDefinition: aRFiDefinition [

	| newBehavior |
	(self isFullyLoadedClassNamed: aRFiDefinition name) ifTrue: [ ^ self ].

	self ensureAllSlotsAreInstanceVariableSlotsIn: aRFiDefinition.

	aRFiDefinition isBehavior ifTrue: [ "We ensure a class with that name already exists"
		environment at: aRFiDefinition name ifAbsent: [ self createStubForClassNamed: aRFiDefinition name ] ].

	newBehavior := self executeDefinitionFor: aRFiDefinition.

	"Once the class is created we set a new method dictionary with a size that will make it grow less afterwards"
	builder bootstrapInterpreter evaluateCode: 'theBehavior methodDictionary: (MethodDictionary new: capacity)' withTemps: {
			('capacity' -> (aRFiDefinition methods size asLiteralInObjectSpace: builder objectSpace)).
			('theBehavior' -> newBehavior) }.

	aRFiDefinition isBehavior
		ifTrue: [ self registerClass: newBehavior asClassMirror named: aRFiDefinition name fullyLoaded: true ]
		ifFalse: [ self registerTrait: newBehavior asTraitMirror named: aRFiDefinition name fullyLoaded: true ]
]

{ #category : 'stubs' }
PBClassLoader >> createClassStubForDefinition: aClassDefinition [
	
	| class metaclass |	
	metaclass := self metaclassClass basicNew asClassMirror.
	metaclass format: (self formatOfClass: aClassDefinition classSide).
	
	class := metaclass basicNew asClassMirror.
	class format: (self formatOfClass: aClassDefinition).
	^ class
]

{ #category : 'stages' }
PBClassLoader >> createJustStubs [

	shouldInitializeClassPool := false.
]

{ #category : 'stubs' }
PBClassLoader >> createStubForClassNamed: aSymbol [

	| class stub |
	class := systemDefinition classNamed: aSymbol.
	^ class isBehavior
		ifTrue: [
			stub := self createClassStubForDefinition: class.
			self registerClass: stub named: aSymbol.
			stub ]
		ifFalse: [ Error signal ].
]

{ #category : 'class-loading' }
PBClassLoader >> createTraitFromDefinition: aTrait [

	| newTrait |	
	newTrait := builder objectSpace interpreter evaluateCode: aTrait definition.
	self registerTrait: newTrait named: aTrait name.
	^ newTrait
]

{ #category : 'class-loading' }
PBClassLoader >> ensureAllSlotsAreInstanceVariableSlotsIn: aRFiDefinition [

	aRFiDefinition slots
		detect: [ :slot | slot isValidForBootstrap not ]
		ifFound: [ :slot |
			self error:
				'Cannot use special slots in Bootstrap. The slot #' , slot name , ' in ' , aRFiDefinition name , ' should be an InstanceVariableSlot instead of: '
				, slot expression ]
]

{ #category : 'accessing' }
PBClassLoader >> environment: anEnvironment [

	environment := anEnvironment
]

{ #category : 'class-loading' }
PBClassLoader >> executeDefinitionFor: aRFiDefinition [

	| definition |
	definition := aRFiDefinition isBehavior ifTrue: [ 
		self classDefinitionFor: aRFiDefinition.
	] ifFalse: [
		aRFiDefinition definition.
	].

	^ builder bootstrapInterpreter evaluateCode: definition.
]

{ #category : 'stubs' }
PBClassLoader >> formatOfClass: aRFiClass [ 
	^ builder formatOfClass: aRFiClass
]

{ #category : 'initialization' }
PBClassLoader >> initialize [

	super initialize.
	initializedClasses := Set new.
	compactClasses := EPDictionary new.
	definitionMirrorMap := EPDictionary new.
	mirrorDefinitionMap := EPDictionary new.
	
	fullyLoaded := Dictionary new
]

{ #category : 'initialization' }
PBClassLoader >> initializeClassNamed: aClassName [

	(self shouldInitializeClassNamed: aClassName) ifFalse: [ ^ self ].
		
	self doInitializeClassNamed: aClassName.
	initializedClasses add: aClassName.
	self postInitializeClassNamed: aClassName
]

{ #category : 'stubs' }
PBClassLoader >> initializeClassPool: aClassName [

	| definition class classPool classVariableBindings |
	definition := systemDefinition classNamed: aClassName.
	class := environment at: aClassName.

	classVariableBindings := definition classVarNames collect: [ :each | | classVar |
		classVar := (self classNamed: #ClassVariable) basicNew.
		classVar instanceVariableAtIndex: 1 put: (each asLiteralInObjectSpace: builder objectSpace).
		classVar instanceVariableAtIndex: 3 put: class "initialize owningClass" ].
	
	class := environment at: aClassName.
	classPool := builder bootstrapInterpreter
		evaluateCode: 'aClassVariableNamesCollection asDictionary'
		withTemps: { 
			'aClassVariableNamesCollection' -> (builder objectSpace newArrayWithAll: classVariableBindings)
		}.
	
	"Force the creation of the symbol using the object space class table." 
	self flag: #bug. "If we do not use basicSetClassName, we duplicate symbols"
	class basicSetClassName: (aClassName asLiteralInObjectSpace: builder objectSpace).
	class classPool: classPool.
	^ class
]

{ #category : 'stages' }
PBClassLoader >> initializeClassPools [

	shouldInitializeClassPool := true.
	environment keysDo: [ :name |
		(systemDefinition classNamed: name) isBehavior
			ifTrue: [ self initializeClassPool: name ] ].
]

{ #category : 'initialization' }
PBClassLoader >> initializeTraitNamed: aClassName [

	(self shouldInitializeClassNamed: aClassName) ifFalse: [ ^ self ].
	self doInitializeTraitNamed: aClassName.
	initializedClasses add: aClassName.
]

{ #category : 'class-loading' }
PBClassLoader >> isClassDefinition: aString [

	^ aString beginsWith: '| newClass |
	newClass := ShiftClassInstaller make: [ :builder |
		builder
			superclass: '.
]

{ #category : 'initialization' }
PBClassLoader >> isFullyLoadedClassNamed: aClassName [

	^ fullyLoaded at: aClassName ifAbsent: [ false ]
]

{ #category : 'class-loading' }
PBClassLoader >> loadClassNamed: aString [
	| class |
	class := self createStubForClassNamed: aString.
	shouldInitializeClassPool
		ifTrue: [ self initializeClassPool: aString ].
	^ class
]

{ #category : 'stubs' }
PBClassLoader >> loadedClassNamed: aString [

	^ environment includesKey: aString
]

{ #category : 'stubs' }
PBClassLoader >> loadedClassNames [
	
	^ environment keys
]

{ #category : 'registration' }
PBClassLoader >> map: aClassMirror with: aDefinition [

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

{ #category : 'stubs' }
PBClassLoader >> metaclassClass [
	
	"Base case for recursion on class creation.
	Ok, so here we really go down and create the metaclass from nothing. We will use this metaclass to create all the others"
	| aClassMapping theNewMetaclass theNewMetaclassClass classFormat metaclassFormat |
	
	environment at: #Metaclass ifPresent: [ :aClass | ^ aClass ].

	aClassMapping := systemDefinition classNamed: #Metaclass.
	classFormat := self formatOfClass: aClassMapping.	
	metaclassFormat := self formatOfClass: aClassMapping classSide.
	
	theNewMetaclass := builder objectSpace
		createClassWithFormat: metaclassFormat
		forInstancesOfFormat: classFormat.
	
	theNewMetaclassClass := builder objectSpace
		createClassWithFormat: classFormat
		forInstancesOfFormat: metaclassFormat.
	
	theNewMetaclass setClass: theNewMetaclassClass.
	theNewMetaclassClass setClass: theNewMetaclass.
	
	self registerClass: theNewMetaclass named: aClassMapping name.
	^ theNewMetaclass
]

{ #category : 'registration' }
PBClassLoader >> migrateOldClass: oldClass to: newClass [

	| oldClassMetaclass |
	
	"We migrate the class pools"
	oldClass asClassMirror classPool isNilObject ifFalse: [
		newClass asClassMirror classPool: oldClass asClassMirror classPool ].
	
	"We migrate the class instance variables"
	"A class has at least 14 instances variables (cf class description, behavior). Starting from 15, we get user defined class instances variables."
	((newClass instanceVariableMapping classNumberOfVariables) + 1) to: oldClass fixedSize do: [ :index |
		newClass instanceVariableAtIndex: index put: (oldClass instanceVariableAtIndex: index).
	].

	"We become the old class and metaclass into the new ones"
	oldClassMetaclass := oldClass basicClass.
	oldClass becomeInto: newClass.
	oldClassMetaclass target == Metaclass
		ifFalse:[ oldClassMetaclass becomeInto: newClass basicClass ] 
]

{ #category : 'registration' }
PBClassLoader >> newClassIndex [
	
	nextClassIndex ifNil: [ nextClassIndex := 1024 ].
	^ nextClassIndex := nextClassIndex + 1
]

{ #category : 'registration' }
PBClassLoader >> registerClass: aClassMirror named: aName [

	^ self registerClass: aClassMirror named: aName fullyLoaded: false
]

{ #category : 'registration' }
PBClassLoader >> registerClass: aClassMirror named: aName fullyLoaded: aBoolean [

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

{ #category : 'registration' }
PBClassLoader >> registerTrait: aTraitMirror named: aName fullyLoaded: aBoolean [

	(environment includesKey: aName)
		ifTrue: [ | oldClass |
			oldClass := environment at: aName.
			oldClass becomeInto: aTraitMirror.
			"oldClassMetaclass target handle == Metaclass
				ifFalse:[ oldClassMetaclass becomeInto: aClassMirror classSide ] "].

	environment at: aName put: aTraitMirror.
	fullyLoaded at: aName put: aBoolean
	"aClassMirror compactClassIndex = 0 ifFalse: [ 
		compactClasses at: aClassMirror compactClassIndex put: aClassMirror ].
	
	objectSpace interpreter resetClassMap.
"
]

{ #category : 'accessing' }
PBClassLoader >> systemDefinition: aRFiEnvironment [ 
	systemDefinition := aRFiEnvironment
]
