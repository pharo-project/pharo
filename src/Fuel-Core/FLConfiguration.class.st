"
I am the abstract superclass of all configurations.

My subclasses implement configuration options specific to certain aspects of the serialization / materialization process.

Any object that needs to access the configuration can do so by using the #TFLConfigurable trait and sending #configuration to itself.
"
Class {
	#name : 'FLConfiguration',
	#superclass : 'Object',
	#instVars : [
		'map'
	],
	#classInstVars : [
		'default'
	],
	#category : 'Fuel-Core-Public',
	#package : 'Fuel-Core',
	#tag : 'Public'
}

{ #category : 'accessing' }
FLConfiguration class >> default [
	^ default ifNil: [ default := self new ]
]

{ #category : 'accessing-defaults' }
FLConfiguration class >> defaultDirectory [
	^ FileSystem disk workingDirectory
]

{ #category : 'accessing-defaults' }
FLConfiguration class >> defaultFileExtension [
	^ 'fuel'
]

{ #category : 'accessing-defaults' }
FLConfiguration class >> defaultFilename [
	^ 'serialization'
]

{ #category : 'accessing-defaults' }
FLConfiguration class >> defaultGlobalEnvironment [
	^ self environment
]

{ #category : 'accessing-defaults' }
FLConfiguration class >> defaultGlobalSymbols [

    ^ #(#Smalltalk #SourceFiles #Transcript #Undeclared #Display #TextConstants #ActiveWorld #ActiveHand #ActiveEvent #Sensor #Processor #SystemOrganization #World)
]

{ #category : 'accessing-defaults' }
FLConfiguration class >> defaultHasMultipleObjects [
	^ false
]

{ #category : 'accessing-defaults' }
FLConfiguration class >> defaultSignature [
	^ 'FUEL'
]

{ #category : 'accessing' }
FLConfiguration class >> new [
	self == FLConfiguration ifTrue: [
		FLConfigurationError signal: 'Use one of my subclasses' ].
	
	^ super new
]

{ #category : 'accessing' }
FLConfiguration >> additionalObjects [
	^ map
		at: #additionalObjects
		ifAbsentPut: [ IdentityDictionary new ]
]

{ #category : 'accessing' }
FLConfiguration >> classAndVariableMigrations [
	^ map 
		at: #classAndVariableMigrations
		ifAbsent: [ Dictionary new ]
]

{ #category : 'configuring-stream' }
FLConfiguration >> decorateStreamFactory: aBlock [
	aBlock isBlock ifFalse: [
		FLConfigurationError signal: 'Stream factory must be a block' ].
	aBlock numArgs = 1 ifFalse: [
		FLConfigurationError signal: 'The block must accept the original stream as single argument' ].
	
	map
		at: #streamFactory
		ifPresent: [ :factory |
			map
				at: #streamFactory
				put: [ aBlock cull: factory value ] ]
		ifAbsent: [
			FLConfigurationError signal: 'No stream factory configured yet. ',
				'Did you mean to use #streamFactory:? ',
				'Or did you forget to supply a stream factory first?' ]
]

{ #category : 'accessing' }
FLConfiguration >> environment [
	^ map
		at: #environment
		ifAbsentPut: [ self class defaultGlobalEnvironment ]
]

{ #category : 'configuring' }
FLConfiguration >> environment: anEnvironment [
	map
		at: #environment
		ifPresent: [ :env | FLConfigurationError signal: 'Environment already set' ]
		ifAbsentPut: [ anEnvironment ].
	
	"We at least need the UndefinedObject class in case we substitute an object with nil
	and serialize the class in an environment without nil"
	anEnvironment
		at: #UndefinedObject
		ifAbsentPut: [ self class environment at: #UndefinedObject ]
]

{ #category : 'configuring-convenience' }
FLConfiguration >> filePath: aString [
	"Takes the path to the file that should be used for serializing or materializing data.
	The path must be a string and may be relative."

	aString isString ifFalse: [ FLConfigurationError signal: 'The file path can only be a string.' ].
	
	self streamFactory: (self streamFactoryForFilePath: aString)
]

{ #category : 'hooks' }
FLConfiguration >> fuelAccept: aGeneralMapper [
	aGeneralMapper
		visitSubstitution: self
		by: nil
]

{ #category : 'testing' }
FLConfiguration >> hasMultipleObjects [
	^ map
		at: #hasMultipleObjects
		ifAbsent: [ self class defaultHasMultipleObjects ]
]

{ #category : 'initialization' }
FLConfiguration >> initialize [
	super initialize.
	
	map := IdentityDictionary new
]

{ #category : 'accessing' }
FLConfiguration >> object [
	^ map
		at: #object
		ifAbsent: [
			FLConfigurationError signal: 'You need to specify the object you want to serialize.' ]
]

{ #category : 'configuring-convenience' }
FLConfiguration >> onStream: aStream [
	"Takes a stream to use for serialization or materialization.
	
	This method allows the same serialization / materialization to be reused with
	different streams.
	
	It is the responsibility of the sender to close the stream."

	aStream isStream ifFalse: [ FLConfigurationError signal: 'Argument is not a stream.' ].
	
	map
		removeKey: #streamFactory ifAbsent: [ ];
		removeKey: #streamFinalizer ifAbsent: [ ].
		
	self
		streamFactory: [ aStream ];
		streamFinalizer: [ :stream | "do not close the stream" ]
]

{ #category : 'testing' }
FLConfiguration >> shouldFinalizeStream [
	^ true
]

{ #category : 'accessing' }
FLConfiguration >> signature [
	^ map
		at: #signature
		ifAbsentPut: [ self class defaultSignature ]
]

{ #category : 'accessing' }
FLConfiguration >> streamFactory [
	^ map
		at: #streamFactory
		ifAbsent: [ FLConfigurationError signal: 'No stream factory provided' ]
]

{ #category : 'configuring-stream' }
FLConfiguration >> streamFactory: aBlock [
	aBlock isBlock ifFalse: [
		FLConfigurationError signal: 'Stream factory must be a block' ].
	aBlock numArgs = 0 ifFalse: [
		FLConfigurationError signal: 'The block must not accept any arguments' ].
	
	map
		at: #streamFactory
		ifPresent: [ :factory |
			FLConfigurationError signal: 'Stream factory already configured. ',
				'Did you mean to use #decorateStreamFactory:?' ]
		ifAbsentPut: [ aBlock ]
]

{ #category : 'private' }
FLConfiguration >> streamFactoryForFilePath: aString [
	^ self subclassResponsibility
]

{ #category : 'accessing' }
FLConfiguration >> streamFinalizer [
	^ map
		at: #streamFinalizer
		ifAbsent: [ [ :stream | stream close] ]
]

{ #category : 'configuring-stream' }
FLConfiguration >> streamFinalizer: aBlock [
	aBlock isBlock ifFalse: [
		FLConfigurationError signal: 'Stream finalizer must be a block' ].
	aBlock numArgs = 1 ifFalse: [
		FLConfigurationError signal: 'The block must accept the stream as single argument' ].
	
	map
		at: #streamFinalizer
		ifPresent: [ :finalizer |
			FLConfigurationError signal: 'Stream finalizer already configured' ]
		ifAbsentPut: [ aBlock ]
]

{ #category : 'private' }
FLConfiguration >> useDuring: aBlock [
	^ FLCurrentConfiguration
		value: self
		during: [
			| context |
			context := FLContext new.
			[ context useDuring: aBlock ] ensure: [
				context finalizeStream ] ]
]

{ #category : 'accessing' }
FLConfiguration >> version [
	^ map
		at: #version
		ifAbsent: [ FLVersion current ]
]
