Class {
	#name : #FLMaterializerTest,
	#superclass : #FLSerializationTest,
	#category : #'Fuel-Tests-Core-Configuration'
}

{ #category : #'tests-configuring' }
FLMaterializerTest >> testDisableMethodChangedWarning [
	self deny: self materializer isMethodChangedWarningDisabled.
	
	self materializer disableMethodChangedWarning.
	self assert: self materializer isMethodChangedWarningDisabled
]

{ #category : #'tests-configuring-migrations' }
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

{ #category : #'tests-configuring-migrations' }
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

{ #category : #'tests-configuring-migrations' }
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

{ #category : #'tests-configuring-migrations' }
FLMaterializerTest >> testMigrateClassNamedToClassSourceAndTargetCanBeTheSame [
	self
		shouldnt: [
			self materializer
				migrateClassNamed: #Object
				toClass: Object ]
		raise: FLConfigurationError
]

{ #category : #'tests-configuring-migrations' }
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

{ #category : #'tests-configuring-migrations' }
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

{ #category : #'tests-configuring-migrations' }
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
