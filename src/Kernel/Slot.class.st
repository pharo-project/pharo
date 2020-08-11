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
	#superclass : #Variable,
	#instVars : [
		'owningClass',
		'definingClass'
	],
	#classInstVars : [
		'slotClassDefinition'
	],
	#category : #'Kernel-Variables'
}

{ #category : #'instance creation' }
Slot class >> asSlot [
	^ self new
]

{ #category : #validating }
Slot class >> checkValidName: aSymbol [

	aSymbol startsWithDigit ifTrue: [ 
		^ InvalidSlotName signalFor: aSymbol ].

	(self isPseudovariableName: aSymbol) ifTrue: [ 
		^ InvalidSlotName signalFor: aSymbol ].
	

	(aSymbol allSatisfy: [ :aCharacter | aCharacter isAlphaNumeric or: [ aCharacter = $_ ] ]) ifFalse: [ 
		^ InvalidSlotName signalFor: aSymbol ]

]

{ #category : #testing }
Slot class >> isPseudovariableName: aSymbol [
	^ #('self' 'true' 'false' 'nil' 'thisContext' 'super') includes: aSymbol
]

{ #category : #testing }
Slot class >> isUsed [
	^super isUsed or: [self slotUsers isNotEmpty ]
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
	(aClass includesSelector: #initialize) ifTrue: [ ^ self ].

	Author
		useAuthor: 'Generated'
		during: [ aClass
				compile:
					'initialize
	super initialize.'
				classified: 'initialization' ]
]

{ #category : #private }
Slot >> ensureSlotInitializationFor: aClass [
	| source |
	(self sendsInitializeSlots: aClass) ifTrue: [ ^ self ].

	self ensureInitalizeMethodExists: aClass.

	source := (aClass >> #initialize) sourceCode allButFirst: 10.
	source := 'initialize
	
	self class initializeSlots: self.' , source.

	Author useAuthor: 'Generated' during: [ aClass compile: source classified: 'initialization' ]
]

{ #category : #finalization }
Slot >> finalize: layout [
	"nothing to do here"
]

{ #category : #comparing }
Slot >> hasSameDefinitionAs: otherSlot [

	"equal definition. Slots an have additional state that is not part of the definitoon
	(e.g. the index in IndexSlot).
	This method then is overriden to not take that state into account"
	^self = otherSlot
	

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
		anySatisfy: [ :node | node binding == self ]
]

{ #category : #testing }
Slot >> isDefinedByOwningClass [

	^ self owningClass = self definingClass
]

{ #category : #testing }
Slot >> isInstanceVariable [
	^ true
]

{ #category : #testing }
Slot >> isReadIn: aCompiledCode [
	^aCompiledCode ast instanceVariableReadNodes
		 anySatisfy: [ :node | node binding == self ]
]

{ #category : #testing }
Slot >> isReferenced [
	^ self owningClass 
		ifNil: [ false ] 
		ifNotNil: [ :class | 
		  class withAllSubclasses anySatisfy: [ :subclass | subclass hasMethodAccessingVariable: self ] ]
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
		anySatisfy: [ :node | node binding == self ]
]

{ #category : #accessing }
Slot >> named: aSymbol [
	"to be polymorhic with slot class"
	self name: aSymbol
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

{ #category : #'meta-object-protocol' }
Slot >> read: anObject [
	^ self subclassResponsibility
]

{ #category : #debugging }
Slot >> readInContext: aContext [
	^self read: aContext receiver
]

{ #category : #accessing }
Slot >> scope: aScope [
	"ignored, subclasses can override to analyze the scope they are to be installed in"
]

{ #category : #private }
Slot >> sendsInitializeSlots: aClass [
	(aClass isTrait or: [ aClass includesSelector: #initialize ])
		ifFalse: [ ^ self sendsInitializeSlots: aClass superclass ].

	"verify implemented here"
	aClass
		compiledMethodAt: #initialize
		ifPresent: [ :method | (method hasLiteral: #initializeSlots:) ifTrue: [ ^ true ] ].

	"if it calls superclass, verify there"
	(aClass isTrait not and: [ aClass >> #initialize hasLiteral: #initialize ])
		ifTrue: [ ^ self sendsInitializeSlots: aClass superclass ].

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

{ #category : #debugging }
Slot >> write: aValue inContext: aContext [
	self write: aValue to: aContext receiver
]

{ #category : #'meta-object-protocol' }
Slot >> write: aValue to: anObject [
	^self subclassResponsibility
]
