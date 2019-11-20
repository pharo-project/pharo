Class {
	#name : #PBClassLoader,
	#superclass : #Object,
	#instVars : [
		'builder',
		'initializedClasses',
		'environment',
		'systemDefinition',
		'compactClasses',
		'shouldInitializeClassPool',
		'useClassBuilder',
		'fullyLoaded',
		'definitionMirrorMap',
		'mirrorDefinitionMap'
	],
	#category : 'Pharo30Bootstrap'
}

{ #category : #accessing }
PBClassLoader >> builder: aBuilder [

	builder := aBuilder
]

{ #category : #'class-loading' }
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
			name: ''{name}'';
			layoutClass: {type};
			slots: ''{instanceVariablesString}'' asSlotCollection;
			sharedVariablesFromString: ''{classVariablesString}'';
			sharedPools: ''{sharedPoolsString}'';
			category: ''{category}'';
			environment: {superClass} environment;
			classSlots: ''{classInstanceVariablesString}'' asSlotCollection ].
	newClass layout compactClassIndex: {compactIndex}.
	newClass'
		format: { 
			'superClass' -> aClass superclass name.
			'name' -> aClass name.
			'type' -> type.
			'instanceVariablesString' -> (' ' join: aClass instVarNames).
			'classVariablesString' -> aClass classVariablesString.
			'sharedPoolsString' -> aClass sharedPoolsString.
			'category' -> aClass category asString.
			'classInstanceVariablesString' -> (' ' join: aClass classSide instVarNames).
			'compactIndex' -> (self compactClassIndexForClass: aClass) } asDictionary.
]

{ #category : #accessing }
PBClassLoader >> classForClassMirror: anEPClassMirror [ 
	
	^ mirrorDefinitionMap at: anEPClassMirror ifAbsent: [ systemDefinition classNamed: #Class ]
]

{ #category : #'class-loading' }
PBClassLoader >> classNamed: aString [ 
	
	| class |
	(self loadedClassNamed: aString)
		ifTrue: [ ^ environment at: aString ].
	class := self loadClassNamed: aString.
	^ class
]

{ #category : #stubs }
PBClassLoader >> compactClassIndexForClass: aRFiClass [ 
	
	^ builder compactClassIndexForClassNamed: aRFiClass name.
]

{ #category : #'class-loading' }
PBClassLoader >> createBehaviorFromDefinition: aRFiDefinition [

	| newBehavior |
	(self isFullyLoadedClassNamed: aRFiDefinition name) ifTrue: [ ^ self ].
	
	aRFiDefinition isBehavior ifTrue: [
		"We ensure a class with that name already exists"
		environment at: aRFiDefinition name ifAbsent: [ self createStubForClassNamed: aRFiDefinition name ]].
	
	newBehavior := self executeDefinitionFor: aRFiDefinition.
	
	"Once the class is created we set a new method dictionary with a size that will make it grow less afterwards"
	builder bootstrapInterpreter 
		evaluateCode: 'theBehavior methodDictionary: (MethodDictionary new: capacity)'
		withTemps: { 
			'capacity' -> (aRFiDefinition methods size asLiteralInObjectSpace: builder objectSpace).
			'theBehavior' -> newBehavior }.
	
	aRFiDefinition isBehavior ifTrue: [ 
		self registerClass: newBehavior asClassMirror named: aRFiDefinition name fullyLoaded: true.
	] ifFalse: [
		self registerTrait: newBehavior asTraitMirror named: aRFiDefinition name fullyLoaded: true.
	]
]

{ #category : #stubs }
PBClassLoader >> createClassStubForDefinition: aClassDefinition [
	
	| class metaclass |	
	metaclass := self metaclassClass basicNew asClassMirror.
	metaclass format: (self formatOfClass: aClassDefinition classSide).
	
	class := metaclass basicNew asClassMirror.
	class format: (self formatOfClass: aClassDefinition).
	^ class
]

{ #category : #stages }
PBClassLoader >> createJustStubs [

	shouldInitializeClassPool := false.
]

{ #category : #stubs }
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

{ #category : #'class-loading' }
PBClassLoader >> createTraitFromDefinition: aTrait [

	| newTrait |	
	newTrait := builder objectSpace interpreter evaluateCode: aTrait definition.
	self registerTrait: newTrait named: aTrait name.
	^ newTrait
]

{ #category : #accessing }
PBClassLoader >> environment: anEnvironment [

	environment := anEnvironment
]

{ #category : #'class-loading' }
PBClassLoader >> executeDefinitionFor: aRFiDefinition [

	| definition |
	definition := aRFiDefinition isBehavior ifTrue: [ 
		self classDefinitionFor: aRFiDefinition.
	] ifFalse: [
		aRFiDefinition definition.
	].

	^ builder bootstrapInterpreter evaluateCode: definition.
]

{ #category : #stubs }
PBClassLoader >> formatOfClass: aRFiClass [ 
	^ builder formatOfClass: aRFiClass
]

{ #category : #initialization }
PBClassLoader >> initialize [

	super initialize.
	initializedClasses := Set new.
	compactClasses := EPDictionary new.
	definitionMirrorMap := EPDictionary new.
	mirrorDefinitionMap := EPDictionary new.
	
	"maybe we could use a state pattern but for now this is so simple"
	useClassBuilder := false.
	
	fullyLoaded := Dictionary new
]

{ #category : #initialization }
PBClassLoader >> initializeClassNamed: aClassName [

	(self shouldInitializeClassNamed: aClassName) ifFalse: [ ^ self ].
		
	self doInitializeClassNamed: aClassName.
	initializedClasses add: aClassName.
	self postInitializeClassNamed: aClassName
]

{ #category : #stubs }
PBClassLoader >> initializeClassPool: aClassName [

	| definition class classPool classVariableBindings |
	definition := systemDefinition classNamed: aClassName.
	classVariableBindings := definition classVarNames collect: [ :each | | classVar |
		classVar := (self classNamed: #ClassVariable) basicNew.
		classVar instanceVariableAtIndex: 1 put: (each asLiteralInObjectSpace: builder objectSpace) ].
	
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

{ #category : #stages }
PBClassLoader >> initializeClassPools [

	shouldInitializeClassPool := true.
	environment keysDo: [ :name |
		(systemDefinition classNamed: name) isBehavior
			ifTrue: [ self initializeClassPool: name ] ].
]

{ #category : #initialization }
PBClassLoader >> initializeTraitNamed: aClassName [

	(self shouldInitializeClassNamed: aClassName) ifFalse: [ ^ self ].
	self doInitializeTraitNamed: aClassName.
	initializedClasses add: aClassName.
]

{ #category : #'class-loading' }
PBClassLoader >> isClassDefinition: aString [

	^ aString beginsWith: '| newClass |
	newClass := ShiftClassInstaller make: [ :builder |
		builder
			superclass: '.
]

{ #category : #initialization }
PBClassLoader >> isFullyLoadedClassNamed: aClassName [

	^ fullyLoaded at: aClassName ifAbsent: [ false ]
]

{ #category : #'class-loading' }
PBClassLoader >> loadClassNamed: aString [
	| class |
	class := self createStubForClassNamed: aString.
	shouldInitializeClassPool
		ifTrue: [ self initializeClassPool: aString ].
	^ class
]

{ #category : #stubs }
PBClassLoader >> loadedClassNamed: aString [

	^ environment includesKey: aString
]

{ #category : #stubs }
PBClassLoader >> loadedClassNames [
	
	^ environment keys
]

{ #category : #registration }
PBClassLoader >> map: aClassMirror with: aDefinition [

	"We create a mapping between the real classes and their definitions for later"
	
	mirrorDefinitionMap at: aClassMirror put: aDefinition.
	definitionMirrorMap at: aDefinition put: aClassMirror.
	
	mirrorDefinitionMap at: aClassMirror basicClass put: aDefinition classSide.
	definitionMirrorMap at: aDefinition classSide put: aClassMirror basicClass
]

{ #category : #stubs }
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

{ #category : #registration }
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

{ #category : #accessing }
PBClassLoader >> next [

	self subclassResponsibility
]

{ #category : #registration }
PBClassLoader >> registerClass: aClassMirror named: aName [

	^ self registerClass: aClassMirror named: aName fullyLoaded: false
]

{ #category : #registration }
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
	
	"We reset the class map of the ast interpreter"
	fullyLoaded at: aName put: aBoolean
]

{ #category : #registration }
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

{ #category : #accessing }
PBClassLoader >> systemDefinition: aRFiEnvironment [ 
	systemDefinition := aRFiEnvironment
]

{ #category : #stages }
PBClassLoader >> useClassBuilder [
	
	useClassBuilder := true
]
