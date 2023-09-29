Class {
	#name : 'FLSerializerTest',
	#superclass : 'FLSerializationTest',
	#category : 'Fuel-Core-Tests-Configuration',
	#package : 'Fuel-Core-Tests',
	#tag : 'Configuration'
}

{ #category : 'tests-configuring-materialization' }
FLSerializerTest >> testAddPostMaterializationAction [
	| block |
	block := [ :x | 1 + 2 ].
	self serializer addPostMaterializationAction: block.
	
	self assert: self serializer postMaterializationActions size equals: 1.
	self assert: self serializer postMaterializationActions anyOne compiledBlock equals: block compiledBlock
]

{ #category : 'tests-configuring-materialization' }
FLSerializerTest >> testAddPostMaterializationActionCanAcceptThreeArguments [
	{
		[].
		[ :a ].
		[ :a :b ].
		[ :a :b :c ]
	} do: [ :block |
		self
			shouldnt: [ self serializer addPostMaterializationAction: block ]
			raise: FLConfigurationError ].
		
	self
		should: [ self serializer addPostMaterializationAction: [ :a :b :c :d ] ]
		raise: FLConfigurationError
]

{ #category : 'tests-configuring-materialization' }
FLSerializerTest >> testAddPostMaterializationActionMustBeBlock [
	{
		Object.
		Object new.
		Object>>#=.
		nil.
		true
	} do: [ :object |
		self
			should: [ self serializer addPostMaterializationAction: object ]
			raise: FLConfigurationError ].
		
	self
		shouldnt: [ self serializer addPostMaterializationAction: [] ]
		raise: FLConfigurationError
]

{ #category : 'tests-configuring-materialization' }
FLSerializerTest >> testAddPostMaterializationActionMustBeCleanBlock [
	self
		should: [ self serializer addPostMaterializationAction: [ self ] ]
		raise: FLConfigurationError.
		
	self
		shouldnt: [ self serializer addPostMaterializationAction: [ Smalltalk ] ]
		raise: FLConfigurationError
]

{ #category : 'tests-configuring-materialization' }
FLSerializerTest >> testAddPreMaterializationAction [
	| block |
	block := [ :x | 1 + 2 ].
	self serializer addPreMaterializationAction: block.
	
	self assert: self serializer preMaterializationActions size equals: 1.
	self assert: self serializer preMaterializationActions anyOne compiledBlock equals: block compiledBlock
]

{ #category : 'tests-configuring-materialization' }
FLSerializerTest >> testAddPreMaterializationActionCanAcceptTwoArguments [
	{
		[].
		[ :a ].
		[ :a :b ]
	} do: [ :block |
		self
			shouldnt: [ self serializer addPreMaterializationAction: block ]
			raise: FLConfigurationError ].
		
	self
		should: [ self serializer addPreMaterializationAction: [ :a :b :c ] ]
		raise: FLConfigurationError
]

{ #category : 'tests-configuring-materialization' }
FLSerializerTest >> testAddPreMaterializationActionMustBeBlock [
	{
		Object.
		Object new.
		Object>>#=.
		nil.
		true
	} do: [ :object |
		self
			should: [ self serializer addPreMaterializationAction: object ]
			raise: FLConfigurationError ].
		
	self
		shouldnt: [ self serializer addPreMaterializationAction: [] ]
		raise: FLConfigurationError
]

{ #category : 'tests-configuring-materialization' }
FLSerializerTest >> testAddPreMaterializationActionMustBeCleanBlock [
	self
		should: [ self serializer addPreMaterializationAction: [ self ] ]
		raise: FLConfigurationError.
		
	self
		shouldnt: [ self serializer addPreMaterializationAction: [ Smalltalk ] ]
		raise: FLConfigurationError
]

{ #category : 'tests-configuring-materialization' }
FLSerializerTest >> testAtPutAdditionalObject [
	self assert: self serializer additionalObjects isEmpty.
	
	self serializer
		at: #foo putAdditionalObject: Object;
		at: #foo putAdditionalObject: Class.
		
	self assert: self serializer additionalObjects size equals: 1.
	self assert: (self serializer additionalObjects at: #foo) identicalTo: Class
]

{ #category : 'tests-configuring' }
FLSerializerTest >> testEnableLimitObjectsException [
	self deny: self serializer isLimitObjectsExceptionEnabled.
	
	self serializer enableLimitObjectsException.
	self assert: self serializer isLimitObjectsExceptionEnabled
]

{ #category : 'tests-configuring-globals' }
FLSerializerTest >> testFullySerializeAllBehaviors [
	| behaviors |
	behaviors := { Object. Object. Array. self classFactory silentlyNewTrait }.
	self serializer fullySerializeAllBehaviors: behaviors.
	
	self assert: self serializer behaviorsToSerialize size equals: 3.
	self assertCollection: self serializer behaviorsToSerialize  hasSameElements: behaviors asSet
]

{ #category : 'tests-configuring-globals' }
FLSerializerTest >> testFullySerializeAllBehaviorsMustBeCollection [
	{
		Object.
		Object new.
		nil.
		true.
		[ :x ] } do: [ :object |
			self
				should: [ self serializer fullySerializeAllBehaviors: object ]
				raise: FLConfigurationError ].
		
	self
		shouldnt: [ self serializer fullySerializeAllBehaviors: Array new ]
		raise: FLConfigurationError
]

{ #category : 'tests-configuring-globals' }
FLSerializerTest >> testFullySerializeAllMethods [
	| methods |
	methods := { Object>>#=. Object>>#=. Object>>#at: }.
	self serializer fullySerializeAllMethods: methods.
	
	self assert: self serializer methodsToSerialize size equals: 2.
	self assertCollection: self serializer methodsToSerialize hasSameElements: methods asSet
]

{ #category : 'tests-configuring-globals' }
FLSerializerTest >> testFullySerializeAllMethodsMustBeCollection [
	{
		Object.
		Object new.
		nil.
		true.
		[ :x ] } do: [ :object |
			self
				should: [ self serializer fullySerializeAllMethods: object ]
				raise: FLConfigurationError ].
		
	self
		shouldnt: [ self serializer fullySerializeAllMethods: Array new ]
		raise: FLConfigurationError
]

{ #category : 'tests-configuring-globals' }
FLSerializerTest >> testFullySerializeBehavior [
	self serializer fullySerializeBehavior: Object.
	
	self assert: self serializer behaviorsToSerialize size equals: 1.
	self assert: self serializer behaviorsToSerialize anyOne identicalTo: Object
]

{ #category : 'tests-configuring-globals' }
FLSerializerTest >> testFullySerializeBehaviorMustBeBehavior [
	{
		Object new.
		nil.
		true.
		[ :x ] } do: [ :object |
			self
				should: [ self serializer fullySerializeBehavior: object ]
				raise: FLConfigurationError ].
	
	self
		shouldnt: [ self serializer fullySerializeBehavior: Object ]
		raise: FLConfigurationError.
		
	self
		shouldnt: [ self serializer fullySerializeBehavior: self classFactory silentlyNewTrait ]
		raise: FLConfigurationError
]

{ #category : 'tests-configuring-globals' }
FLSerializerTest >> testFullySerializeBehaviorUsesSet [
	self serializer
		fullySerializeBehavior: Object;
		fullySerializeBehavior: Object.
	
	self assert: self serializer behaviorsToSerialize size equals: 1
]

{ #category : 'tests-configuring-globals' }
FLSerializerTest >> testFullySerializeMethod [
	self serializer fullySerializeMethod: Object>>#=.
	
	self assert: self serializer methodsToSerialize size equals: 1.
	self assert: self serializer methodsToSerialize anyOne identicalTo: Object>>#=
]

{ #category : 'tests-configuring-globals' }
FLSerializerTest >> testFullySerializeMethodMustBeMethod [
	{
		Object new.
		nil.
		true.
		[ :x ] } do: [ :object |
			self
				should: [ self serializer fullySerializeMethod: object ]
				raise: FLConfigurationError ].
	
	self
		shouldnt: [ self serializer fullySerializeMethod: Object>>#= ]
		raise: FLConfigurationError
]

{ #category : 'tests-configuring-globals' }
FLSerializerTest >> testFullySerializeMethodUsesSet [
	self serializer
		fullySerializeMethod: Object>>#=;
		fullySerializeMethod: Object>>#=.
	
	self assert: self serializer methodsToSerialize size equals: 1
]

{ #category : 'tests-configuring' }
FLSerializerTest >> testLimitDepthTo [
	self deny: self serializer hasDepthLimit.

	self serializer limitDepthTo: -4.
	self deny: self serializer hasDepthLimit.
	
	self serializer limitDepthTo: 0.
	self deny: self serializer hasDepthLimit.
	
	self serializer limitDepthTo: 1.
	self assert: self serializer hasDepthLimit
]

{ #category : 'tests-configuring' }
FLSerializerTest >> testLimitObjectsTo [
	self deny: self serializer hasObjectsLimit.

	self serializer limitObjectsTo: -4.
	self deny: self serializer hasObjectsLimit.
	
	self serializer limitObjectsTo: 0.
	self deny: self serializer hasObjectsLimit.
	
	self serializer limitObjectsTo: 1.
	self assert: self serializer hasObjectsLimit
]

{ #category : 'tests-configuring-globals' }
FLSerializerTest >> testLookUpAllInGlobalsMustBeCollection [
	{
		Object.
		Object new.
		nil.
		true.
		[ :x ] } do: [ :object |
			self
				should: [ self serializer lookUpAllInGlobals: object ]
				raise: FLConfigurationError ].
		
	self
		shouldnt: [ self serializer lookUpAllInGlobals: Array new ]
		raise: FLConfigurationError
]

{ #category : 'tests-configuring-globals' }
FLSerializerTest >> testLookUpInGlobalsMustBeGlobal [
	| value |
	"Ensure that we do not use the system environment for this test"
	self testingEnvironment: FLSystemGlobalsTestResource current globals.
	
	value := Object new.
	self serializer lookUpInGlobals: value.
	self
		should: [ self serializer serialize ]
		raise: FLConfigurationError.
		
	self environmentOfTest
		at: #aGlobal
		put: value.
	addedGlobals add: #aGlobal.
	serializer := FLSerializer new
		streamFactory: [ ByteArray new writeStream ];
		object: Object new;
		lookUpInGlobals: value;
		yourself.
	self
		should: [ serializer serialize ]
		raise: FLConfigurationError.
		
	serializer := FLSerializer new
		streamFactory: [ ByteArray new writeStream ];
		object: Object new;
		environment: self environmentOfTest;
		lookUpInGlobals: value;
		yourself.
	self
		shouldnt: [ serializer serialize ]
		raise: FLConfigurationError
]

{ #category : 'tests-configuring-globals' }
FLSerializerTest >> testLookUpInGlobalsSymbolKey [
	| object |
	object := Object new.
	self environmentOfTest
		at: #aGlobal
		put: object.
	addedGlobals add: #aGlobal.
	self serializer lookUpInGlobals: #aGlobal.
	
	self assert: (self serializer globals includesKey: #aGlobal).
	self assert: (self serializer globals at: #aGlobal) identicalTo: object.
	self deny: (self serializer globals includesKey: object)
]

{ #category : 'tests-configuring-globals' }
FLSerializerTest >> testLookupAllInGlobals [
	| globals global1 marge |
	global1 := Object new.
	marge := 'Marge'.
	globals := { Object. Class. Smalltalk. #World. global1. marge }.
	self serializer lookUpAllInGlobals: globals.
	self environmentOfTest
		at: #global1 put: global1;
		at: #global2 put: marge.
	addedGlobals
		add: #global1;
		add: #global2.
	
	{
		#Object -> Object.
		#Class -> Class.
		#Smalltalk -> Smalltalk.
		#World -> World.
		#global1 -> global1.
	} do: [ :assoc |
		self assert: (self serializer globals includesAssociation: assoc) ]
]

{ #category : 'tests-configuring' }
FLSerializerTest >> testObject [
	| object |
	object := Object new.
	self serializer object: object.
	self assert: self serializer object identicalTo: object
]

{ #category : 'tests-configuring' }
FLSerializerTest >> testObjectCollection [
	| array |
	array := Array new: 0.
	self deny: self serializer hasMultipleObjects.
	
	self serializer objectCollection: array.
	self assert: self serializer object identicalTo: array.
	self assert: self serializer hasMultipleObjects
]

{ #category : 'tests-configuring' }
FLSerializerTest >> testObjectCollectionMustBeCollection [
	{
		Object new.
		Object.
		WriteStream basicNew.
		ReadStream basicNew
	} do: [ :object |
		self
			should: [ FLSerializer new objectCollection: object ]
			raise: FLConfigurationError ].
	
	
	{
		Array new: 0.
		OrderedCollection new.
		Set new.
		Bag new
	} do: [ :collection |
		self
			shouldnt: [ FLSerializer new objectCollection: collection ]
			raise: FLConfigurationError ]
]

{ #category : 'tests-configuring' }
FLSerializerTest >> testObjectCollectionWhenAlreadySet [
	serializer := FLSerializer new
		objectCollection: (Array new: 0);
		yourself.
	
	self
		should: [ serializer objectCollection: (Array new: 0) ]
		raise: FLConfigurationError
]

{ #category : 'tests-configuring' }
FLSerializerTest >> testObjectWhenAlreadySet [
	serializer := FLSerializer new
		object: Object new;
		yourself.
	
	self
		should: [ serializer object: Object new ]
		raise: FLConfigurationError
]

{ #category : 'tests-configuring' }
FLSerializerTest >> testWhenSubstituteBy [
	| condition substitute substitution |
	condition := [ :x | x - 3 ].
	substitute := [ :y | 1 + 2 ].
	self assert: self serializer substitutions isEmpty.
	
	self serializer
		when: condition
		substituteBy: substitute.
		
	self deny: self serializer substitutions isEmpty.
	self assert: self serializer substitutions size equals: 1.
	
	substitution := self serializer substitutions anyOne.
	self assert: substitution key compiledBlock identicalTo: condition compiledBlock.
	self assert: substitution value compiledBlock identicalTo: substitute compiledBlock
]

{ #category : 'tests-configuring' }
FLSerializerTest >> testWhenSubstituteByMustAcceptSingleArgument [
	{
		[].
		[ :a :b ].
		[ :a :b :c ]
	} do: [ :block |
		self
			should: [
				self serializer
					when: [ :x ]
					substituteBy: block ]
			raise: FLConfigurationError.
		self
			should: [
				self serializer
					when: block
					substituteBy: [ :x ] ]
			raise: FLConfigurationError ].
		
	self
		shouldnt: [
			self serializer
				when: [ :x ]
				substituteBy: [ :y ] ]
		raise: FLConfigurationError
]

{ #category : 'tests-configuring' }
FLSerializerTest >> testWhenSubstituteByMustBeBlocks [
	{
		Object -> [ :x ].
		Object new -> [ :x ].
		(Array new: 0) -> [ :x ].
		(1 -> 2) -> (1 -> 2)
	} do: [ :assoc |
		self
			should: [
				self serializer
					when: assoc key
					substituteBy: assoc value ]
			raise: FLConfigurationError.
		self
			should: [
				self serializer
					when: assoc value
					substituteBy: assoc key ]
			raise: FLConfigurationError ].
	
	self
		shouldnt: [
			self serializer
				when: [ :x ]
				substituteBy: [ :x ] ]
		raise: FLConfigurationError
]
