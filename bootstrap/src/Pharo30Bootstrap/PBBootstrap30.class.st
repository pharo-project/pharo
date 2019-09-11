Class {
	#name : #PBBootstrap30,
	#superclass : #Object,
	#instVars : [
		'objectSpace',
		'classLoader',
		'systemDefinition',
		'bootstrapInterpreter',
		'bootstrappedBindingCache',
		'bootstrapEnvironment',
		'remoteMetaclassBindings'
	],
	#category : 'Pharo30Bootstrap'
}

{ #category : #bootstrapping }
PBBootstrap30 class >> bootstrap [

	self bootstrap: '3.864'
]

{ #category : #bootstrapping }
PBBootstrap30 class >> bootstrap: version [

	self bootstrap: version toFileNamed: version, '.image'
]

{ #category : #bootstrapping }
PBBootstrap30 class >> bootstrap: version toFileNamed: aFileName [

	| definition bootstrap edit |
	definition := (FiMetacelloWriter new written: (ConfigurationOfPharoBootstrap project version: version)).

	edit := definition ficusPackages 
		editAtPath: FiPath root / #Slot / #contents / #SlotClassBuilder / #methods
		do: [ :elem | elem editInclude: (FiMethod sourceCode: 'validateClassName') ].
	definition := (definition ficusPackages resultOf: edit) asRFiEnvironment.
	
	edit := definition ficusPackages
		editAtPath: FiPath root / #Traits / #contents / #TClass / #methods
		do: [ :elem | elem editInclude: (FiMethod sourceCode: 'setName: aSymbol andRegisterInCategory: categorySymbol environment: aSystemDictionary	
	(self environment == aSystemDictionary
		and: [self name = aSymbol
			and: [self category = categorySymbol]]) ifTrue: [^self].

	(self environment notNil and: [self name notNil and: [self name ~= aSymbol]]) ifTrue: [
		self environment renameClass: self as: aSymbol].
	
	self name: aSymbol.
	self environment: aSystemDictionary.	
	self environment at: self name put: self.
	self environment organization classify: self name under: categorySymbol.
	^ true') ].
	definition := (definition ficusPackages resultOf: edit) asRFiEnvironment.

	edit := definition ficusPackages
		editAtPath: FiPath root / #Traits / #contents / #TBehavior / #methods
		do: [ :elem | elem editInclude: (FiMethod sourceCode: 'addTraitSelector: aSymbol withMethod: aCompiledMethod
	| source method |
	method := aCompiledMethod copy.
	method selector: aSymbol.
	method methodClass: self.
	self isTrait ifFalse: [ method properties at: #traitSource put: aCompiledMethod ].
	self basicAddSelector: aSymbol withMethod: method') ].
	definition := (definition ficusPackages resultOf: edit) asRFiEnvironment.

	bootstrap := self basicNew
		initializeSystemDefinition: definition;
		initializeBootstrapEnvironment;
		createVMStubs;
		createInitialObjects;
		createClasses;
		installMethods;
		initializeImage.
		
	bootstrap objectSpace serializeInFileNamed: aFileName
]

{ #category : #visiting }
PBBootstrap30 >> behaviorFrom: aRFiClassOrTrait [ 
	^ aRFiClassOrTrait accept: self
]

{ #category : #'bootstrapping-bindings' }
PBBootstrap30 >> bindingOf: aString [

	self halt.
	^ bootstrappedBindingCache
		at: aString
		ifAbsentPut: [ 
			| association |
			association := (self classNamed: #Association) mirror basicNew.
			association instanceVariableAtIndex: 1 put: self objectSpace nilObject.
			association instanceVariableAtIndex: 2 put: (self classNamed: aString) mirror.
			association ]
]

{ #category : #'bootstrapping-bindings' }
PBBootstrap30 >> bindingOfGlobal: aString [

	self halt.
	^ bootstrappedBindingCache
		at: aString
		ifAbsentPut: [ 
			| association |
			association := (self classNamed: #Association) basicNew.
			association instanceVariableAtIndex: 1 put: self objectSpace nilObject.
			association instanceVariableAtIndex: 2 put: (self classNamed: aString).
			association ]
]

{ #category : #accessing }
PBBootstrap30 >> bindingOfMetaclassNamed: aBehaviorName [

	^remoteMetaclassBindings
		at: aBehaviorName
		ifAbsentPut: [ objectSpace
			newAssociationWithKey: objectSpace nilObject
			andValue: (bootstrapEnvironment at: aBehaviorName) basicClass ]
]

{ #category : #running }
PBBootstrap30 >> bootstrap [

	self initializeBootstrapEnvironment.
	self createVMStubs.
	self createInitialObjects.
	self createClasses.
	self installMethods.
	self initializeImage.
]

{ #category : #accessing }
PBBootstrap30 >> bootstrapEnvironment [
	
	^ bootstrapEnvironment
]

{ #category : #accessing }
PBBootstrap30 >> bootstrapInterpreter [
	
	^ bootstrapInterpreter
]

{ #category : #'class-loader' }
PBBootstrap30 >> builtClassNames [
	
	^ classLoader loadedClassNames
]

{ #category : #helpers }
PBBootstrap30 >> checkpoint: name [

	| isNewSession |
	isNewSession := Smalltalk saveAs: name.
	isNewSession ifTrue: [ UIManager default defer: [ self halt ]]
]

{ #category : #accessing }
PBBootstrap30 >> classForClassMirror: anEPClassMirror [

	^ classLoader classForClassMirror: anEPClassMirror
]

{ #category : #accessing }
PBBootstrap30 >> classLoader [
	^ classLoader
]

{ #category : #'class-loader' }
PBBootstrap30 >> classNamed: aString [ 
	
	^ EPRemoteClass on: (classLoader classNamed: aString) environment: self
]

{ #category : #helpers }
PBBootstrap30 >> compactClassIndexForClassNamed: aName [
	
	^ #(
	CompiledMethod
	Slot
	Array
	LargeNegativeInteger
	LargePositiveInteger
	Float
	Protocol
	Association
	Point
	Rectangle
	ByteString
	BlockClosure
	nil
	MethodContext
	nil
	Bitmap
	nil
	nil
	nil
	nil
	nil
	nil
	nil
	nil
	nil
	nil
	nil
	nil
	nil
	nil
	nil) indexOf: aName
]

{ #category : #running }
PBBootstrap30 >> createBehaviorFromDefinition: aRFiDefinition [

	^ classLoader createBehaviorFromDefinition: aRFiDefinition
]

{ #category : #running }
PBBootstrap30 >> createClasses [
	| definitions total smalltalkInstance |
	definitions := systemDefinition allClassesAndTraits sorted: [ :a :b | a name < b name ].
	total := definitions size.
	definitions
		withIndexDo: [ :aClassDefinition :index | | time |
			time := [self createBehaviorFromDefinition: aClassDefinition.] timeToRun.
			
			self log: ((index printPaddedWith: $0 to: 3), '/' , 
								total asString, ' - ', 
									time asString ,': Built behavior ', 
										aClassDefinition name). ].
	
	smalltalkInstance := bootstrapEnvironment at: #Smalltalk.
	objectSpace interpreter: bootstrapInterpreter.
	objectSpace backend smalltalkInstance: smalltalkInstance.
	objectSpace environment: ((EPSystemDictionary withSystemDictionary: objectSpace backend systemDictionary) objectSpace: objectSpace; yourself).
	self checkpoint: 'created_classes'.
]

{ #category : #running }
PBBootstrap30 >> createFalse [

	objectSpace falseObject: (self createStubForClassNamed: #False) basicNew.
	^ objectSpace falseObject
]

{ #category : #running }
PBBootstrap30 >> createInitialObjects [
	
	| characterTable |
	"We add the main globals that will be later needed for compilation"
	self log: 'initializing globals'.
	self bootstrapEnvironment at: #Processor put: objectSpace nilObject.
	self bootstrapEnvironment at: #Smalltalk put: objectSpace nilObject.
	self bootstrapEnvironment at: #Undeclared put: objectSpace nilObject.
	
	"We initialize the hash table sizes by hand.
	The AST interpreter is too slow for it"
	self log: 'initializing hash table sizes'.
	(self classNamed: #HashTableSizes) mirror
		instanceVariableAtIndex: 15
		put: (objectSpace newArrayWithAll: (HashTableSizes sizes collect: [:each | objectSpace objectMirrorOn: each ])).
	
	self log: 'preparing class builder'.
	self bootstrapInterpreter evaluateCode: 'DangerousClassNotifier disable'.
	self bootstrapInterpreter evaluateCode: 'Undeclared := Dictionary new.'.
	self bootstrapInterpreter evaluateCode: 'Smalltalk := SmalltalkImage basicNew.'.
	self bootstrapInterpreter evaluateCode: 'Smalltalk instVarAt: 1 put: (SystemDictionary withOrganizer: SystemOrganizer new).'.
	self bootstrapInterpreter evaluateCode: 'Smalltalk at: #Smalltalk put: Smalltalk.'.
	self bootstrapInterpreter evaluateCode: 'Smalltalk at: #Undeclared put: Undeclared.'.
	
	self log: 'class loader now creates class pools'.
	classLoader initializeClassPools.

	self log: 'Initializing StartUp list'.
	self bootstrapInterpreter evaluateCode: 'SmalltalkImage initialize.'.
	
	self log: 'Initializing Character Table'.
	self bootstrapInterpreter evaluateCode: 'Character initialize.'.
	characterTable := (EPInternalCharacterTable new objectSpace: objectSpace).
	objectSpace backend characterTable: ((self classNamed: #Character) classPool at: #CharacterTable).
	objectSpace characterTable: characterTable.
	
	self log: 'Initializing Character Table'.
	self bootstrapInterpreter evaluateCode: 'String classPool at: #AsciiOrder put: ((0 to: 255) as: ByteArray).'.
	
	self log: 'Initializing Date and Time constants'.
	self bootstrapInterpreter evaluateCode: 'DateAndTime initialize.'.
	self bootstrapInterpreter evaluateCode: 'ChronologyConstants initialize.'.
	
	self log: 'Initializing object dependent fields'.
	self bootstrapInterpreter evaluateCode: 'Object initialize.'.
	
	self log: 'Installing symbol table'.
	self bootstrapInterpreter evaluateCode: 'Symbol initialize.
	Symbol classPool at: #SymbolTable put: (WeakSet withAll: ByteSymbol allInstances)'.
	objectSpace symbolTable: (EPInternalSymbolTable new objectSpace: objectSpace).
	
	self log: 'Bootstrapping class layouts'.
	bootstrapInterpreter evaluateCode:
		'| superLayout |
		superLayout := EmptyLayout instance.
		Class
			instVarAt: 4
			put: (FixedLayout
					extending: superLayout 
					scope: (superLayout slotScope extend: #(superclass methodDict format layout instanceVariables organization subclasses name classPool sharedPools environment category traitComposition localSelectors))
					host: Class).
		Class
			instVarAt: 5 put: #(superclass methodDict format layout instanceVariables organization subclasses name classPool sharedPools environment category traitComposition localSelectors).
			
		SmallInteger
			instVarAt: 4
			put: (SmallIntegerLayout new).'.
			
	self log: 'Class builder ready to create classes'.
	classLoader useClassBuilder.
	self checkpoint: 'created_initial_objects'.
]

{ #category : #running }
PBBootstrap30 >> createNil [

	"Temporal nil: it is needed so the nil stub class can be initialized with it"
	objectSpace nilObject: (objectSpace objectMirrorOn: (UndefinedObject basicNew)).
	
	"We create the real nil and become it"
	objectSpace nilObject becomeInto: (self createStubForClassNamed: #UndefinedObject) basicNew.
	
	^ objectSpace nilObject
]

{ #category : #'class-loader' }
PBBootstrap30 >> createStubForClassNamed: aString [ 
	
	^ classLoader createStubForClassNamed: aString
]

{ #category : #running }
PBBootstrap30 >> createTrue [

	objectSpace trueObject: (self createStubForClassNamed: #True) basicNew.
	^ objectSpace trueObject
]

{ #category : #running }
PBBootstrap30 >> createVMStubs [

	self log: 'creating initial Objects needed by the VM'.
	
	objectSpace backend specialObjectsArray: (objectSpace objectMirrorOn: (Array new: 59)).
	self createNil.
	1 to: 59 do: [ :i |
		objectSpace backend specialObjectsArray at: i put: objectSpace nilObject ].
	
	self createFalse.
	self createTrue.
	objectSpace backend smallIntegerClass: (self createStubForClassNamed: #SmallInteger).
	objectSpace backend characterClass: (self createStubForClassNamed: #Character).
	objectSpace backend byteSymbolClass: (self createStubForClassNamed: #ByteSymbol).
	objectSpace backend byteStringClass: (self createStubForClassNamed: #ByteString).
	objectSpace backend byteArrayClass: (self createStubForClassNamed: #ByteArray).
	objectSpace backend associationClass: (self createStubForClassNamed: #Association).
	objectSpace backend arrayClass: (self createStubForClassNamed: #Array).
	objectSpace backend symbolTableClass: (self createStubForClassNamed: #Symbol).
	objectSpace backend largeNegativeIntegerClass: (self createStubForClassNamed: #LargeNegativeInteger).
	objectSpace backend largePositiveIntegerClass: (self createStubForClassNamed: #LargePositiveInteger).
	objectSpace backend methodClass: (self createStubForClassNamed: #CompiledMethod).
	objectSpace backend floatClass: (self createStubForClassNamed: #Float).
	self checkpoint: 'created_vm_objects'.
]

{ #category : #running }
PBBootstrap30 >> definitionNamed: aName [

	^ systemDefinition classNamed: aName
]

{ #category : #accessing }
PBBootstrap30 >> globalNamed: aString [ 
	
	(self includesClassNamed: aString)
		ifTrue: [ ^ (self classNamed: aString) mirror ].
	self halt.
]

{ #category : #accessing }
PBBootstrap30 >> globalNamed: aString put: aValue [
	
	bootstrapEnvironment at: aString put: aValue
]

{ #category : #testing }
PBBootstrap30 >> includesClassNamed: aString [
	
	^ systemDefinition includesClassNamed: aString
]

{ #category : #testing }
PBBootstrap30 >> includesGlobalNamed: aString [ 
	
	"It is a global if we have it in our system definition"
	^ systemDefinition includesClassNamed: aString.
]

{ #category : #running }
PBBootstrap30 >> initialize [

	super initialize.
	systemDefinition ifNil: [ 
		systemDefinition := (FiMetacelloWriter new written: 
			(ConfigurationOfPharoBootstrap project version: '3.864')) ].
	bootstrappedBindingCache := EPDictionary new.
	remoteMetaclassBindings := EPDictionary new.
]

{ #category : #running }
PBBootstrap30 >> initializeBootstrapEnvironment [
	
	bootstrapEnvironment := Dictionary new.
	
	"We initialize the object space with a boot symbol table and character table.
	We will later replace them by real ones."
	objectSpace := EPObjectSpace new.
	objectSpace backend: EPOzVMBackend new.
	objectSpace characterTable: (EPExternalCharacterTable new objectSpace: objectSpace; yourself).
	objectSpace symbolTable: (EPExternalSymbolTable new objectSpace: objectSpace; yourself).

	classLoader createJustStubs.
	classLoader environment: bootstrapEnvironment.
	classLoader systemDefinition: systemDefinition.
	classLoader builder: self.
	
	bootstrapInterpreter := EPASTEvaluator new.
	bootstrapInterpreter codeProvider: self.
	bootstrapInterpreter environment: self.
	bootstrapInterpreter objectSpace: objectSpace.
	objectSpace interpreter: bootstrapInterpreter.
]

{ #category : #running }
PBBootstrap30 >> initializeImage [
	self halt: 'should migrate this method'.
	self checkpoint: 'Image initialized'.
]

{ #category : #running }
PBBootstrap30 >> initializeSystemDefinition: aSystemDefinition [

	systemDefinition := aSystemDefinition.
	self initialize.
]

{ #category : #running }
PBBootstrap30 >> installMethods [
	
	| definitions total |
	definitions := systemDefinition allClassesAndTraits sorted: [ :a :b | a name > b name ].
	total := definitions size.
	definitions withIndexDo: [ :aClassDefinition :index |
		self installMethodsInBehaviorDefinition: aClassDefinition prefixed: (index printPaddedWith: $0 to: 3), '/' , total asString.
	].
	self checkpoint: 'installed_methods'.
]

{ #category : #running }
PBBootstrap30 >> installMethodsInBehaviorDefinition: behaviorDefinition prefixed: aPrefix [
	| behaviorMirror  |
	behaviorMirror := bootstrapEnvironment at: behaviorDefinition name.
"	1haltIf: [ behaviorMirror isKindOf: EPTraitMirror ] ."
	self
		installMethodsInBehaviorMapping: behaviorDefinition
		using: self methodBuilder
		onMirror: behaviorMirror
		prefixed: aPrefix.
	self
		installMethodsInBehaviorMapping: behaviorDefinition classSide
		using: self methodBuilder
		onMirror: behaviorMirror basicClass
		prefixed: aPrefix.
]

{ #category : #running }
PBBootstrap30 >> installMethodsInBehaviorMapping: aBehaviorMapping using: aMethodBuilder onMirror: mirror prefixed: aPrefix [
	| newMethods theClass |
	newMethods := aMethodBuilder methodsFromDefinition: aBehaviorMapping.
	newMethods := newMethods sorted: [ :m1 :m2 | m1 selector < m2 selector ].
	theClass := (self classNamed: aBehaviorMapping theNonMetaClass name) mirror.
	aBehaviorMapping isMeta ifTrue: [ theClass := theClass basicClass ].
	newMethods do: [ :m |
		self log: aPrefix, ': Installing method ', 
			aBehaviorMapping name, '>>', m selector.
		self objectSpace interpreter
			evaluateCode: 'theBehavior addSelectorSilently: selector withMethod: compiledMethod'
			withTemps: { 
				'theBehavior' -> theClass.
				'selector' -> (m selector asLiteralInObjectSpace: objectSpace).
				'compiledMethod' -> m }
	].
]

{ #category : #testing }
PBBootstrap30 >> isMeta: aMirror [
	
	^ (self classNamed: #Metaclass) mirror = aMirror basicClass
]

{ #category : #helpers }
PBBootstrap30 >> log: aString [

	FileStream stdout nextPutAll: aString;cr.
]

{ #category : #accessing }
PBBootstrap30 >> metaclassForClassMirror: anEPClassMirror [

	^ EPRemoteMetaClass on: anEPClassMirror environment: self
]

{ #category : #accessing }
PBBootstrap30 >> methodBuilder [
	^ PBMethodCompiler new builder: self; yourself.	
]

{ #category : #accessing }
PBBootstrap30 >> objectSpace [
	^ objectSpace
]

{ #category : #visiting }
PBBootstrap30 >> visitClass: aRFiClass [
	^ EPRemoteClass 
		on: (classLoader classNamed: aRFiClass name) 
		environment: self
]

{ #category : #visiting }
PBBootstrap30 >> visitTrait: aRFiTrait [ 
	^ EPRemoteTrait 
		on: (classLoader classNamed: aRFiTrait name)
		environment: self
]
