Class {
	#name : 'FLConfigurationTest',
	#superclass : 'FLSerializationTest',
	#category : 'Fuel-Core-Tests-Configuration',
	#package : 'Fuel-Core-Tests',
	#tag : 'Configuration'
}

{ #category : 'tests-configuring-stream' }
FLConfigurationTest >> testDecorateStreamFactory [

	| byteConverterStream string |
	byteConverterStream := self classFactory silentlyMake: [ :aBuilder | aBuilder superclass: DecoratorStream ].
	self classFactory
		silentlyCompile: 'nextPut: anInteger
			decoratedStream nextPut: (Character value: anInteger)'
		in: byteConverterStream.

	string := String new: 3.
	serializer := FLSerializer new
		              streamFactory: [ string writeStream ];
		              decorateStreamFactory: [ :stream | byteConverterStream on: stream ];
		              yourself.

	serializer streamFactory value
		nextPut: $f asciiValue;
		nextPut: $o asciiValue;
		nextPut: $o asciiValue.
	self assert: string equals: 'foo'.

	string := String new: 3.
	serializer := FLMaterializer new
		              streamFactory: [ string writeStream ];
		              decorateStreamFactory: [ :stream | byteConverterStream on: stream ];
		              yourself.

	serializer streamFactory value
		nextPut: $f asciiValue;
		nextPut: $o asciiValue;
		nextPut: $o asciiValue.
	self assert: string equals: 'foo'
]

{ #category : 'tests-configuring-stream' }
FLConfigurationTest >> testDecorateStreamFactoryMustAcceptSingleArgument [
	{
		[].
		[ :a :b ].
		[ :a :b :c ]
	} do: [ :block |
		self
			should: [ self serializer decorateStreamFactory: block ]
			raise: FLConfigurationError.
		self
			should: [ self materializer decorateStreamFactory: block ]
			raise: FLConfigurationError ].
		
	self
		shouldnt: [ self serializer decorateStreamFactory: [ :a ] ]
		raise: FLConfigurationError.
	self
		shouldnt: [ self materializer decorateStreamFactory: [ :a ] ]
		raise: FLConfigurationError
]

{ #category : 'tests-configuring-stream' }
FLConfigurationTest >> testDecorateStreamFactoryMustBeBlock [
	{
		#streamFactory:.
		Object new.
		Object
	} do: [ :block |
		self
			should: [ self serializer decorateStreamFactory: block ]
			raise: FLConfigurationError.
		self
			should: [ self materializer decorateStreamFactory: block ]
			raise: FLConfigurationError ].
		
	self
		shouldnt: [ self serializer decorateStreamFactory: [ :a ] ]
		raise: FLConfigurationError.
	self
		shouldnt: [ self serializer decorateStreamFactory: [ :a ] ]
		raise: FLConfigurationError
]

{ #category : 'tests-configuring-stream' }
FLConfigurationTest >> testDecorateStreamFactoryWhenStreamFactoryNotSet [
	self
		should: [ FLSerializer new decorateStreamFactory: [ :stream ] ]
		raise: FLConfigurationError.
	self
		should: [ FLMaterializer new decorateStreamFactory: [ :stream ] ]
		raise: FLConfigurationError
]

{ #category : 'tests-configuring' }
FLConfigurationTest >> testEnvironment [
	| materializerEnvironment serializerEnvironment |
	serializerEnvironment := Dictionary new.
	materializerEnvironment := Dictionary new.
	serializer := FLSerializer new
		environment: serializerEnvironment;
		yourself.
	materializer := FLMaterializer new
		environment: materializerEnvironment;
		yourself.
	
	self assert: serializerEnvironment identicalTo: serializer environment.
	self assert: materializerEnvironment identicalTo: materializer environment.
	self deny: serializer environment identicalTo: materializer environment
]

{ #category : 'tests-configuring' }
FLConfigurationTest >> testEnvironmentDefault [
	self assert: FLSerializer new environment identicalTo: self class environment.
	self assert: FLMaterializer new environment identicalTo: self class environment
]

{ #category : 'tests-configuring' }
FLConfigurationTest >> testEnvironmentHasAtLeastUndefinedObject [
	serializer := FLSerializer new
		environment: Dictionary new;
		yourself.
	materializer := FLMaterializer new
		environment: Dictionary new;
		yourself.
		
	self assert: (serializer environment includesKey: #UndefinedObject).
	self assert: (serializer environment at: #UndefinedObject) identicalTo: UndefinedObject.
	self assert: (materializer environment includesKey: #UndefinedObject).
	self assert: (materializer environment at: #UndefinedObject) identicalTo: UndefinedObject
]

{ #category : 'tests-configuring' }
FLConfigurationTest >> testEnvironmentWhenAlreadySet [
	serializer := FLSerializer new
		environment: self environmentOfTest;
		yourself.
	materializer := FLMaterializer new
		environment: self environmentOfTest;
		yourself.

	self
		should: [ serializer environment: Dictionary new ]
		raise: FLConfigurationError.
		
	self
		should: [ materializer environment: Dictionary new ]
		raise: FLConfigurationError
]

{ #category : 'tests-configuring-convenience' }
FLConfigurationTest >> testFilePath [
	| fileName materialized |
	fileName := 'test_file_name'.
	self serializer
		filePath: fileName;
		object: 'abc'.
	
	self deny: (FLFilePoolTestResource current fileExists: fileName).
	self serializer serialize.
	
	self assert: (FLFilePoolTestResource current fileExists: fileName).
	
	materialized := self materializer
		filePath: fileName;
		materializeRoot.
	self assert: materialized equals: 'abc'
]

{ #category : 'tests-configuring-convenience' }
FLConfigurationTest >> testFilePathWhenStreamFactoryAlreadySet [
	serializer := FLSerializer new
		filePath: 'filename';
		yourself.
	 self
		should: [ serializer filePath: 'filename' ]
		raise: FLConfigurationError
]

{ #category : 'tests-configuring-convenience' }
FLConfigurationTest >> testFilePathWrong [
	 self
		should: [ self serializer filePath: 'filename' asFileReference ]
		raise: FLConfigurationError.
	self
		shouldnt: [ self serializer filePath: 'filename' ]
		raise: FLConfigurationError.
	self
		shouldnt: [ self serializer filePath: 'filename' asSymbol ]
		raise: FLConfigurationError
]

{ #category : 'tests-configuring-convenience' }
FLConfigurationTest >> testOnStream [
	| stream materialized |
	stream := (ByteArray new: 100) writeStream.
	self serializer
		onStream: stream;
		object: 'abc';
		serialize.
	
	self deny: stream isEmpty.
	
	materialized := self materializer
		onStream: stream contents readStream;
		materializeRoot.
	self assert: materialized equals: 'abc'
]

{ #category : 'tests-configuring-convenience' }
FLConfigurationTest >> testOnStreamDoesNotCloseStream [
	| stream |
	stream := FLFilePoolTestResource current writeStreamForFilePath: 'test'.
	self serializer
		onStream: stream;
		object: 'abc';
		serialize.
	
	self deny: stream closed.
	
	stream close.
	self assert: stream closed
]

{ #category : 'tests-configuring-convenience' }
FLConfigurationTest >> testOnStreamWithReuse [
	| stream1 stream2 materialized |
	stream1 := (ByteArray new: 5) writeStream.
	stream2 := (ByteArray new: 5) writeStream.
	self serializer
		onStream: stream1;
		object: 'abc';
		serialize.
		
	self serializer
		onStream: stream2;
		object: 'efg';
		serialize.
	
	self deny: stream1 isEmpty.
	self deny: stream2 isEmpty.
	
	materialized := self materializer
		onStream: stream1 contents readStream;
		materializeRoot.
	self assert: materialized equals: 'abc'.
	
	materialized := self materializer
		onStream: stream2 contents readStream;
		materializeRoot.
	self assert: materialized equals: 'efg'
]

{ #category : 'tests-configuring-convenience' }
FLConfigurationTest >> testOnStreamWrong [
	 self
		should: [ self serializer onStream: 'filename' ]
		raise: FLConfigurationError.
		
	self
		should: [ self serializer onStream: nil ]
		raise: FLConfigurationError
]

{ #category : 'tests-configuring-stream' }
FLConfigurationTest >> testStreamFactory [
	| string |
	string := String new: 3.
	serializer := FLSerializer new
		streamFactory: [ string writeStream ];
		yourself.
	materializer := FLMaterializer new
		streamFactory: [ string readStream ];
		yourself.
	
	serializer streamFactory value nextPutAll: 'foo'.
	self assert: string equals: 'foo'.
	
	self assert: materializer streamFactory value upToEnd equals: 'foo'
]

{ #category : 'tests-configuring-stream' }
FLConfigurationTest >> testStreamFactoryDefault [
	self
		should: [ FLSerializer new streamFactory ]
		raise: FLConfigurationError.
	self
		should: [ FLMaterializer new streamFactory ]
		raise: FLConfigurationError
]

{ #category : 'tests-configuring-stream' }
FLConfigurationTest >> testStreamFactoryMustAcceptNoArguments [
	{
		[ :a ].
		[ :a :b ].
		[ :a :b :c ]
	} do: [ :block |
		self
			should: [ FLSerializer new streamFactory: block ]
			raise: FLConfigurationError.
		self
			should: [ FLMaterializer new streamFactory: block ]
			raise: FLConfigurationError ].
		
	self
		shouldnt: [ FLSerializer new streamFactory: [] ]
		raise: FLConfigurationError.
	self
		shouldnt: [ FLMaterializer new streamFactory: [] ]
		raise: FLConfigurationError
]

{ #category : 'tests-configuring-stream' }
FLConfigurationTest >> testStreamFactoryMustBeBlock [
	{
		#streamFactory:.
		Object new.
		Object
	} do: [ :block |
		self
			should: [ FLSerializer new streamFactory: block ]
			raise: FLConfigurationError.
		self
			should: [ FLMaterializer new streamFactory: block ]
			raise: FLConfigurationError ].
		
	self
		shouldnt: [ FLSerializer new streamFactory: [] ]
		raise: FLConfigurationError.
	self
		shouldnt: [ FLMaterializer new streamFactory: [] ]
		raise: FLConfigurationError
]

{ #category : 'tests-configuring-stream' }
FLConfigurationTest >> testStreamFactoryWhenAlreadySet [
	self
		should: [ self serializer streamFactory: [] ]
		raise: FLConfigurationError.
	self
		should: [ self materializer streamFactory: [] ]
		raise: FLConfigurationError
]

{ #category : 'tests-configuring-stream' }
FLConfigurationTest >> testStreamFinalizer [
	| contents |
	serializer := FLSerializer new
		streamFinalizer: [ :stream | contents := stream contents ]
		yourself.
	
	contents := self serializer streamFinalizer value: 'foo' readStream.
	self assert: contents equals: 'foo'
]

{ #category : 'tests-configuring-stream' }
FLConfigurationTest >> testStreamFinalizerDefault [

	<ignoreNotImplementedSelectors: #( _log )>
	| logClass log |
	logClass := self classFactory silentlyMake: [ :aBuilder | aBuilder slotsFromString: 'log' ].
	self classFactory
		silentlyCompile: 'initialize log := OrderedCollection new' in: logClass;
		silentlyCompile: '_log ^ log' in: logClass;
		silentlyCompile: 'doesNotUnderstand: aMessage log add: aMessage selector. ^ self' in: logClass.
	log := logClass new.
	FLSerializer new streamFinalizer value: log.
	self assert: log _log size equals: 1.
	self assert: log _log first equals: #close.

	log := logClass new.
	FLMaterializer new streamFinalizer value: log.
	self assert: log _log size equals: 1.
	self assert: log _log first equals: #close
]

{ #category : 'tests-configuring-stream' }
FLConfigurationTest >> testStreamFinalizerMustAcceptSingleArgument [
	{
		[].
		[ :a :b ].
		[ :a :b :c ]
	} do: [ :block |
		self
			should: [ FLSerializer new streamFinalizer: block ]
			raise: FLConfigurationError.
		self
			should: [ FLMaterializer new streamFinalizer: block ]
			raise: FLConfigurationError ].
		
	self
		shouldnt: [ FLSerializer new streamFinalizer: [ :a ] ]
		raise: FLConfigurationError.
	self
		shouldnt: [ FLMaterializer new streamFinalizer: [ :a ] ]
		raise: FLConfigurationError
]

{ #category : 'tests-configuring-stream' }
FLConfigurationTest >> testStreamFinalizerMustBeBlock [
	{
		#streamFinalizer:.
		Object new.
		Object
	} do: [ :block |
		self
			should: [ FLSerializer new streamFinalizer: block ]
			raise: FLConfigurationError.
		self
			should: [ FLMaterializer new streamFinalizer: block ]
			raise: FLConfigurationError ].
		
	self
		shouldnt: [ FLSerializer new streamFinalizer: [ :a ] ]
		raise: FLConfigurationError.
	self
		shouldnt: [ FLMaterializer new streamFinalizer: [ :a ] ]
		raise: FLConfigurationError
]

{ #category : 'tests-configuring-stream' }
FLConfigurationTest >> testStreamFinalizerWhenAlreadySet [
	self serializer streamFinalizer: [ :x ].
	self materializer streamFinalizer: [ :x ].
	
	self
		should: [ self serializer streamFinalizer: [ :x ] ]
		raise: FLConfigurationError.	
	self
		should: [ self materializer streamFinalizer: [ :x ] ]
		raise: FLConfigurationError.
]
