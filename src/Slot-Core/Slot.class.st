"
I'm a meta-object for accessing a slot in an Object. 

I define a protocol to read (#read:) and to write (#write:to:) values. 

For customizing a subclass can override the meta-object-protocol methods. See subclasses for examples.

Vocabulary:
- variable: named accessor for a Slot
- Slot: class-side meta-object, mapping of names to values using a MOP to fields
- field: space occupied in an object, used to hold values accessed via Slots
"
Class {
	#name : #Slot,
	#superclass : #Object,
	#instVars : [
		'name',
		'definingClass'
	],
	#classVars : [
		'Properties'
	],
	#classInstVars : [
		'slotClassDefinition'
	],
	#category : #'Slot-Core-Variables'
}

{ #category : #'instance creation' }
Slot class >> asSlot [
	^ self new
]

{ #category : #validating }
Slot class >> checkValidName: aSymbol [
	aSymbol isPseudovariableName ifTrue: [ 
		^ InvalidSlotName signalFor: aSymbol ]
]

{ #category : #'class initialization' }
Slot class >> initialize [
	Properties
		ifNil: [ self initializeProperties ]
		ifNotNil: [ | newDict | 
			newDict := WeakIdentityKeyDictionary newFrom: Properties.
			newDict rehash.
			Properties := newDict ]
]

{ #category : #'class initialization' }
Slot class >> initializeProperties [
	Properties := WeakIdentityKeyDictionary new.
]

{ #category : #'instance creation' }
Slot class >> named: aSymbol [
	self checkValidName: aSymbol.
	^ self new
		name: aSymbol;
		yourself
]

{ #category : #settings }
Slot class >> showSlotClassDefinition [
	^slotClassDefinition ifNil: [ slotClassDefinition := false ]
]

{ #category : #settings }
Slot class >> showSlotClassDefinition: aBoolean [
	slotClassDefinition := aBoolean
]

{ #category : #settings }
Slot class >> slotSetting: aBuilder [ 
	<systemsettings>
	(aBuilder setting: #showSlotClassDefinition) 
		label: 'Class Template with Slots';
		description: 'If checked then browsers show always the slot class definition';
		parent: #codeBrowsing; 
		target: self
]

{ #category : #comparing }
Slot >> = other [
	self == other
		ifTrue: [ ^ true ].
	^ (self species == other species) 
			and: [ name = other name ]
]

{ #category : #private }
Slot >> addSlotInitToInitialize: aClass [
	|  source |
	
	((aClass>>#initialize) hasLiteral: #initializeSlots:)ifTrue: [^self ].
	
	source := (aClass>>#initialize) sourceCode.
	(source endsWith: '.') ifFalse: [ source := source, '.' ].
	source := source, '
	self class initializeSlots: self.'.
	Author useAuthor: 'Generated' during: [aClass compile: source classified: 'initialization']. 
	
]

{ #category : #converting }
Slot >> asSlot [
	^ self
]

{ #category : #'class building' }
Slot >> changingIn: aClass [
	"the slot was changed. Default is to do nothing"
]

{ #category : #queries }
Slot >> definingClass [
	^definingClass
]

{ #category : #queries }
Slot >> definingClass: aClass [
	definingClass := aClass
]

{ #category : #printing }
Slot >> definitionString [
	"for Slots, the printString is the defintion"
	^self printString
]

{ #category : #'code generation' }
Slot >> emitStore: aMethodBuilder [
	| tempName |
	tempName := Object new.
	aMethodBuilder
		addTemp: tempName;
		storeTemp: tempName;
		popTop;
		pushReceiver;
		pushLiteralVariable: #slot -> self;
		pushTemp: tempName;
		send: #writeSlot:value:
]

{ #category : #'code generation' }
Slot >> emitValue: aMethodBuilder [
	aMethodBuilder
		pushLiteralVariable: #slot -> self;
		pushReceiver;
		send: #read:
]

{ #category : #private }
Slot >> ensureInitalizeMethodExists: aClass [
	(aClass includesSelector:  #initialize) ifTrue: [ ^self ].
	Author useAuthor: 'Generated' during: [
		aClass compile: 'initialize
	super initialize.' classified: 'initialization'.
		] 
	
]

{ #category : #properties }
Slot >> ensureProperties [
	^ Properties at: self ifAbsentPut: WeakKeyDictionary new
]

{ #category : #private }
Slot >> ensureSlotIniatializationFor: aClass [
	self ensureInitalizeMethodExists: aClass. 
	self addSlotInitToInitialize: aClass.
]

{ #category : #finalization }
Slot >> finalize: layout [
	"nothing to do here"
]

{ #category : #properties }
Slot >> hasProperty: aKey [
	"Test if the property aKey is present."
	^self properties 
		ifNil: [false]
		ifNotNil: [:prop | prop includesKey: aKey]
]

{ #category : #comparing }
Slot >> hash [
	^ self species hash bitXor: self name hash
]

{ #category : #initialization }
Slot >> initialize: instance [
	"nothing to do for the default slot"
]

{ #category : #'class building' }
Slot >> installingIn: aClass [
	"I am called by the class builder. This way a Slot can change the class it is installed in"
	self wantsInitalization ifTrue: [ self ensureSlotIniatializationFor: aClass ].
]

{ #category : #testing }
Slot >> isGlobal [
	^false
]

{ #category : #testing }
Slot >> isReadIn: aCompiledCode [
	^aCompiledCode readsSlot: self
]

{ #category : #testing }
Slot >> isSelfEvaluating [
	^true
]

{ #category : #testing }
Slot >> isSpecial [
	"I am more than just a backward compatible ivar slot"
	^true
]

{ #category : #testing }
Slot >> isVirtual [
	"virtual slots do not take up space in the object and have size = 0"
	^true
]

{ #category : #testing }
Slot >> isVisible [
	^ true
]

{ #category : #testing }
Slot >> isWrittenIn: aCompiledCode [
	^aCompiledCode writesSlot: self
]

{ #category : #'class building' }
Slot >> layoutChanged: aClass [
	"I am called by the class builder. This way a Slot can change the class it is installed in"
	
]

{ #category : #accessing }
Slot >> name [
	^ name
]

{ #category : #accessing }
Slot >> name: aSymbol [
	name := aSymbol
]

{ #category : #printing }
Slot >> printOn: aStream [
	aStream 
		store: self name;
		nextPutAll: ' => ';
		nextPutAll: self class name
]

{ #category : #properties }
Slot >> properties [
	^ Properties at: self ifAbsent: nil
]

{ #category : #properties }
Slot >> propertyAt: propName [
	^ self
		propertyAt: propName
		ifAbsent: [ nil ]
]

{ #category : #properties }
Slot >> propertyAt: propName ifAbsent: aBlock [
	self properties ifNil: [^aBlock value].
	^ self properties
		at: propName
		ifAbsent: aBlock
]

{ #category : #properties }
Slot >> propertyAt: aKey ifAbsentPut: aBlock [
	"Answer the property associated with aKey or, if aKey isn't found store the result of evaluating aBlock as new value."
	
	^ self propertyAt: aKey ifAbsent: [ self propertyAt: aKey put: aBlock value ]
]

{ #category : #properties }
Slot >> propertyAt: propName put: propValue [
	^ self ensureProperties
		at: propName
		put: propValue
]

{ #category : #'meta-object-protocol' }
Slot >> read: anObject [
	^ self subclassResponsibility
]

{ #category : #properties }
Slot >> removePropertiesIfEmpty [
	^ Properties at: self ifPresent: [ :dict |
		dict ifEmpty: [ Properties removeKey: self ] ]
]

{ #category : #properties }
Slot >> removeProperty: propName [
	^ self
		removeProperty: propName
		ifAbsent: [ nil ]
]

{ #category : #properties }
Slot >> removeProperty: propName ifAbsent: aBlock [
	| property |
	self properties ifNil: [^aBlock value].
	property := self properties
		removeKey: propName
		ifAbsent: aBlock.
	self removePropertiesIfEmpty.
	^ property
]

{ #category : #'class building' }
Slot >> removingFrom: aClass [
	"I am called by the class builder. This way a Slot can change the class it is installed in"
]

{ #category : #accessing }
Slot >> scope: aScope [
	"ignored, subclasses can override to analyze the scope they are to be installed in"
]

{ #category : #accessing }
Slot >> size [
	"normally a slot takes one ivar. Virtual slots do not take space. 
	 We could even have Slots that map to multiple ivars"
	^self isVirtual ifTrue: [0] ifFalse: [1]
]

{ #category : #printing }
Slot >> storeOn: aStream [
	^self printOn: aStream
]

{ #category : #queries }
Slot >> usingMethods [
	"All methods that read or write the slot"
	^self definingClass 
		ifNil: [ #() ]
		ifNotNil: [:class | class allMethodsAccessingSlot: self].
]

{ #category : #'meta-object-protocol' }
Slot >> wantsInitalization [
	"if a slot wants to hable initalization, return true here"
	^false
]

{ #category : #'meta-object-protocol' }
Slot >> write: aValue to: anObject [
	^self subclassResponsibility
]
