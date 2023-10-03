Class {
	#name : 'FLMaterializerTest',
	#superclass : 'FLSerializationTest',
	#category : 'Fuel-Core-Tests-Configuration',
	#package : 'Fuel-Core-Tests',
	#tag : 'Configuration'
}

{ #category : 'tests-configuring' }
FLMaterializerTest >> testDisableMethodChangedWarning [
	self deny: self materializer isMethodChangedWarningDisabled.
	
	self materializer disableMethodChangedWarning.
	self assert: self materializer isMethodChangedWarningDisabled
]

{ #category : 'tests-running' }
FLMaterializerTest >> testMaterialize [
	| object materialized |
	object := FLPair new
		left: 'a';
		right: 'b';
		yourself.
	self serializer
		at: #homer putAdditionalObject: 'simpson';
		object: object;
		serialize.
	
	materialized := self materializer materialize.
	self assert: FLMaterializedObjects equals: materialized class.
	self assert: 'simpson' equals: (materialized header additionalObjectAt: #homer).
	self assert: object equals: materialized root.
	self assert: 1 equals: materialized roots size
]

{ #category : 'tests-running' }
FLMaterializerTest >> testMaterializeHeader [
	| object materialized |
	object := FLPair new
		left: 'a';
		right: 'b';
		yourself.
	self serializer
		at: #homer putAdditionalObject: 'simpson';
		object: object;
		serialize.
	
	materialized := self materializer materializeHeader.
	self assert: 'simpson' equals: (materialized additionalObjectAt: #homer)
]

{ #category : 'tests-running' }
FLMaterializerTest >> testMaterializeObjectCollection [
	| object1 object2 materialized |
	object1 := FLPair new
		left: 'a';
		right: 'b';
		yourself.
	object2 := FLPair new
		left: 'c';
		yourself.
	self serializer
		at: #homer putAdditionalObject: 'simpson';
		objectCollection: {object1. object2};
		serialize.
	
	materialized := self materializer materializeObjectCollection.
	self assert: FLMaterializedObjects equals: materialized class.
	self assert: 'simpson' equals: (materialized header additionalObjectAt: #homer).
	self assert: object2 equals: materialized root.
	self assert: 2 equals: materialized roots size.
	self assert: object1 equals: materialized roots first.
	self assert: object2 equals: materialized roots second
]

{ #category : 'tests-running' }
FLMaterializerTest >> testMaterializeRoot [
	| object materialized |
	object := FLPair new
		left: 'a';
		right: 'b';
		yourself.
	self serializer
		object: object;
		serialize.
	
	materialized := self materializer materializeRoot.
	self assert: object equals: materialized
]

{ #category : 'tests-configuring-migrations' }
FLMaterializerTest >> testMigrateClassNamedToClass [
	| migration |
	self assert: self materializer migrations isEmpty.
	
	self materializer
		migrateClassNamed: #FLPair
		toClass: Object.
	
	self assert: self materializer migrations size equals: 1.
	
	migration := self materializer migrations anyOne.
	self assert: migration sourceClassName identicalTo: #FLPair.
	self assert: migration targetClass identicalTo: Object
]

{ #category : 'tests-configuring-migrations' }
FLMaterializerTest >> testMigrateClassNamedToClassMustBeSymbolAndClassOrSymbol [
	{
		Object new -> Object.
		Object -> Object.
		nil -> Object.
		true -> Object.
		Array new -> Object
	} do: [ :assoc |
		self
			should: [
				FLMaterializer new
					migrateClassNamed: assoc key
					toClass: assoc value ]
			raise: FLConfigurationError ].
		
	{
		#Object -> Object new.
		#Object -> nil.
		#Object -> true.
		#Object -> Array new
	} do: [ :assoc |
		self
			should: [
				FLMaterializer new
					migrateClassNamed: assoc key
					toClass: assoc value ]
			raise: FLConfigurationError ].
		
	self
		should: [
			self materializer
				migrateClassNamed: #Object
				toClass: #Smalltalk ]
		raise: FLConfigurationError.
		
	{
		#Object -> Class.
		'Object' -> Class.
		#Object -> #Class.
		'Object' -> 'Class'
	} do: [ :assoc |
		self
			shouldnt: [
				FLMaterializer new
					migrateClassNamed: assoc key
					toClass: assoc value ]
			raise: FLConfigurationError ]
]

{ #category : 'tests-configuring-migrations' }
FLMaterializerTest >> testMigrateClassNamedToClassMustHaveUniqueSource [
	self materializer
		migrateClassNamed: #Object
		toClass: Class.
		
	self
		should: [
			self materializer
				migrateClassNamed: #Object
				toClass: Behavior ]
		raise: FLConfigurationError
]

{ #category : 'tests-configuring-migrations' }
FLMaterializerTest >> testMigrateClassNamedToClassSourceAndTargetCanBeTheSame [
	self
		shouldnt: [
			self materializer
				migrateClassNamed: #Object
				toClass: Object ]
		raise: FLConfigurationError
]

{ #category : 'tests-configuring-migrations' }
FLMaterializerTest >> testMigrateClassNamedToClassVariables [
	| migration |
	self assert: self materializer migrations isEmpty.
	
	self materializer
		migrateClassNamed: #FLPair
		toClass: Object
		variables: Dictionary new.
	
	self assert: self materializer migrations size equals: 1.
	
	migration := self materializer migrations anyOne.
	self assert: migration sourceClassName identicalTo: #FLPair.
	self assert: migration targetClass identicalTo: Object
]

{ #category : 'tests-configuring-migrations' }
FLMaterializerTest >> testMigrateClassNamedVariables [
	| migration |
	self assert: self materializer migrations isEmpty.
	
	self materializer
		migrateClassNamed: #FLPair
		variables: Dictionary new.
	
	self assert: self materializer migrations size equals: 1.
	
	migration := self materializer migrations anyOne.
	self assert: migration sourceClassName identicalTo: #FLPair.
	self assert: migration targetClass identicalTo: FLPair
]

{ #category : 'tests-configuring-migrations' }
FLMaterializerTest >> testMigrateClassNamedVariablesMustBeSymbolAndDictionary [
	{
		Object new -> Dictionary new.
		Object -> Dictionary new.
		nil -> Dictionary new.
		true -> Dictionary new.
		Array new -> Dictionary new
	} do: [ :assoc |
		self
			should: [
				FLMaterializer new
					migrateClassNamed: assoc key
					variables: assoc value ]
			raise: FLConfigurationError ].
		
	{
		#Object -> Object.
		#Object -> Object new.
		#Object -> nil.
		#Object -> true.
		#Object -> Array new
	} do: [ :assoc |
		self
			should: [
				FLMaterializer new
					migrateClassNamed: assoc key
					variables: assoc value ]
			raise: FLConfigurationError ].
		
	{
		#Object -> Dictionary new.
		'Object' -> Dictionary new.
		#Object -> IdentityDictionary new.
		'Object' -> IdentityDictionary new
	} do: [ :assoc |
		self
			shouldnt: [
				FLMaterializer new
					migrateClassNamed: assoc key
					variables: assoc value ]
			raise: FLConfigurationError ]
]
