"
I'm a meta-object for accessing a slot in an Object. 

I define a reflective protocol to read (#read:) and to write (#write:to:) values. 

	For efficiency, subclasses (e.g. InstanceVariableSlot) override #emitStore: and #emitValue: to not call #read: and/or write:to:

For customizing a subclass can override the meta-object-protocol methods. See subclasses for examples. If your subclass holds state, the following must be overridden:
- Printing: #printOn: must print the source code string to reconstruct an instance. This will be displayed in the class definition. 
- Equality: #= and #hash must be customized taking account of additional state.


Vocabulary:
- Field: space occupied in an object, used to hold values accessed via Slots
- Slot: class-side meta-object, implements reading and writing.

My state:
name: I have a name (a symbol).
owningClass: the class (or Trait) I am installed in
definingClass: When I get installed via a Trait, owningClass will be the class and
the Trait I am coming from (that defined me) is stored here.


"
Class {
	#name : #Slot,
	#superclass : #Object,
	#instVars : [
		'name',
		'owningClass',
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

	aSymbol startsWithDigit ifTrue: [ 
		^ InvalidSlotName signalFor: aSymbol ].

	aSymbol isPseudovariableName ifTrue: [ 
		^ InvalidSlotName signalFor: aSymbol ].
	

	(aSymbol allSatisfy: [ :aCharacter | aCharacter isAlphaNumeric or: [ aCharacter = $_ ] ]) ifFalse: [ 
		^ InvalidSlotName signalFor: aSymbol ]

]

{ #category : #'class initialization' }
Slot class >> initialize [
	Properties := Properties
		ifNil: [ WeakIdentityKeyDictionary new. ]
		ifNotNil: [ (WeakIdentityKeyDictionary newFrom: Properties) rehash]
]

{ #category : #testing }
Slot class >> isUsed [
	^super isUsed or: [self slotUsers isNotEmpty ]
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
		default: false;
		description: 'If checked then browsers show always the slot class definition';
		parent: #codeBrowsing; 
		target: self
]

{ #category : #testing }
Slot class >> slotUsers [
	"all classes or traits that have slots of this kind"
	^self environment allBehaviors 
		select:  [ :class | class slots anySatisfy: [ :slot | slot class == self ] ]
]

{ #category : #comparing }
Slot >> = other [
	"Every subclass that adds state must redefine this method"
	self == other
		ifTrue: [ ^ true ].
	^ (self species == other species) 
			and: [ name = other name ]
]

{ #category : #private }
Slot >> addSlotInitToInitialize: aClass [
	|  source |

	(self sendsInitializeSlots: aClass) ifTrue: [ ^ self ].
	
	source := (aClass>>#initialize) sourceCode allButFirst: 10.
	source := 'initialize
	
	self class initializeSlots: self.', 
	source.

	Author 
		useAuthor: 'Generated' 
		during: [ aClass compile: source classified: 'initialization' ]
]

{ #category : #'class building' }
Slot >> asClassVariable [
	self
		error:
			'Slots can not be used to define Class Variables, you need to create a LiteralVariable subclass instead'
]

{ #category : #converting }
Slot >> asSlot [
	^ self
]

{ #category : #accessing }
Slot >> baseSlot [
	^self
]

{ #category : #accessing }
Slot >> definingClass [
	"if a Slot from a Trait is installed in a class, we store the orginal class of the trait here"
	^ definingClass ifNil: [ self owningClass ]
]

{ #category : #accessing }
Slot >> definingClass: aClass [
	definingClass := aClass
]

{ #category : #printing }
Slot >> definitionString [
	"Every subclass that adds state must redefine either this method or #printOn:"
	^ self printString
]

{ #category : #'code generation' }
Slot >> emitStore: aMethodBuilder [
	"generate bytecode to call the reflective write method of the Slot"
	| tempName |
	tempName := '0slotTempForStackManipulation'.  
	aMethodBuilder
		addTemp: tempName;
		storeTemp: tempName;
		popTop;
		pushReceiver;
		pushLiteralVariable: (AdditionalBinding key: #slot value: self);
		pushTemp: tempName;
		send: #writeSlot:value:
]

{ #category : #'code generation' }
Slot >> emitValue: aMethodBuilder [
	aMethodBuilder
		pushLiteralVariable: (AdditionalBinding key: #slot value: self);
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
Slot >> ensureSlotInitializationFor: aClass [
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
Slot >> hasSameDefinitionAs: otherSlot [

	"equal definition. Slots an have additional state that is not part of the definitoon
	(e.g. the index in IndexSlot).
	This method then is overriden to not take that state into account"
	^self = otherSlot
	

]

{ #category : #comparing }
Slot >> hash [
	"Every subclass that adds state must redefine this method"
	^ self species hash bitXor: self name hash
]

{ #category : #initialization }
Slot >> initialize: instance [
	"nothing to do for the default slot"
]

{ #category : #'class building' }
Slot >> installingIn: aClass [
	"I am called by the class builder. This way a Slot can change the class it is installed in"
	self wantsInitialization ifTrue: [ self ensureSlotInitializationFor: aClass ].
]

{ #category : #testing }
Slot >> isAccessedIn: aCompiledCode [
	^aCompiledCode ast instanceVariableNodes
		anySatisfy: [ :node | node binding slot == self ]
]

{ #category : #testing }
Slot >> isGlobal [
	^false
]

{ #category : #testing }
Slot >> isLocal [

	^ self owningClass = self definingClass
]

{ #category : #testing }
Slot >> isReadIn: aCompiledCode [
	^aCompiledCode ast instanceVariableReadNodes
		 anySatisfy: [ :node | node binding slot == self ]
]

{ #category : #testing }
Slot >> isReferenced [
	^self usingMethods isNotEmpty
]

{ #category : #testing }
Slot >> isSelfEvaluating [
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
	^aCompiledCode ast instanceVariableWriteNodes
		anySatisfy: [ :node | node binding slot == self ]
]

{ #category : #accessing }
Slot >> name [
	^ name
]

{ #category : #accessing }
Slot >> name: aSymbol [
	name := aSymbol
]

{ #category : #accessing }
Slot >> named: aSymbol [
	"to be polymorhic with slot class"
	self name: aSymbol
]

{ #category : #testing }
Slot >> needsFullDefinition [
	"I am more than just a backward compatible ivar slot, I need a full definition "
	^true
]

{ #category : #accessing }
Slot >> owningClass [
	"the class that the slot is installed in"
	^owningClass
]

{ #category : #accessing }
Slot >> owningClass: aClass [
	owningClass := aClass
]

{ #category : #printing }
Slot >> printOn: aStream [
	"Every subclass that adds state must redefine either this method or #definitionString"
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

{ #category : #accessing }
Slot >> scope: aScope [
	"ignored, subclasses can override to analyze the scope they are to be installed in"
]

{ #category : #private }
Slot >> sendsInitializeSlots: aClass [
		
	aClass isTrait ifFalse: [ 
		(aClass includesSelector: #initialize) ifFalse: [ 
			^ self sendsInitializeSlots: aClass superclass ] ].
	"verify implemented here"
	((aClass>>#initialize) hasLiteral: #initializeSlots:) 
		ifTrue: [ ^ true ].
	"if it calls superclass, verify there"
	aClass isTrait ifFalse: [ 
		((aClass>>#initialize) hasLiteral: #initialize) 
			ifTrue: [ ^ self sendsInitializeSlots: aClass superclass ] ].
	"is not implemented"
	^ false
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
	^self owningClass 
		ifNil: [ #() ]
		ifNotNil: [:class | class allMethodsAccessingSlot: self].
]

{ #category : #'meta-object-protocol' }
Slot >> wantsInitialization [
	"if a slot wants to enable instance initalization, return true here"
	^false
]

{ #category : #'meta-object-protocol' }
Slot >> write: aValue to: anObject [
	^self subclassResponsibility
]
