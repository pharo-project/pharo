"
I am the configuration for materialization.

I hold all the configuration information necessary for materializing objects.
"
Class {
	#name : 'FLMaterializer',
	#superclass : 'FLConfiguration',
	#category : 'Fuel-Core-Public',
	#package : 'Fuel-Core',
	#tag : 'Public'
}

{ #category : 'accessing-defaults' }
FLMaterializer class >> defaultMethodChangedWarningDisabled [
	^ false
]

{ #category : 'accessing-defaults' }
FLMaterializer class >> defaultShouldMaterializeHeaderOnly [
	^ false
]

{ #category : 'convenience' }
FLMaterializer class >> materializeFrom: aStream [
	^ self new
		onStream: aStream;
		materializeRoot
]

{ #category : 'convenience' }
FLMaterializer class >> materializeFromByteArray: aByteArray [
	^ self materializeFrom: aByteArray readStream
]

{ #category : 'convenience' }
FLMaterializer class >> materializeFromFileNamed: aFilePath [
	^ self new
		filePath: aFilePath;
		materializeRoot

]

{ #category : 'convenience' }
FLMaterializer class >> materializeHeaderFrom: aStream [
	^ self new
		onStream: aStream;
		materializeHeader
]

{ #category : 'convenience' }
FLMaterializer class >> materializeHeaderFromByteArray: aByteArray [
	^ self materializeHeaderFrom: aByteArray readStream
]

{ #category : 'convenience' }
FLMaterializer class >> materializeHeaderFromFileNamed: aFilePath [
	^ self new
		filePath: aFilePath;
		materializeHeader
]

{ #category : 'private' }
FLMaterializer >> basicMigrateClassNamed: aSymbol toClass: aClass variables: aDictionary [
	self
		validateMigrationFrom: aSymbol
		to: aClass.
	
	self migrations add: (FLMigration 
		fromClassNamed: aSymbol 
		toClass: aClass
		variables: aDictionary)
]

{ #category : 'configuring' }
FLMaterializer >> disableMethodChangedWarning [
	"Setting this to true will prevent Fuel from signalling FLMethodChanged exceptions
	when the method of a block has changed upon materialization."

	map
		at: #methodChangedWarningDisabled
		put: true
]

{ #category : 'testing' }
FLMaterializer >> isMethodChangedWarningDisabled [
	^ map
		at: #methodChangedWarningDisabled
		ifAbsent: [ self class defaultMethodChangedWarningDisabled ]
]

{ #category : 'running' }
FLMaterializer >> materialize [
	^ self useDuring: [
		FLMaterialization run ]
]

{ #category : 'running' }
FLMaterializer >> materializeHeader [
	map
		at: #shouldMaterializeHeaderOnly
		put: true.
		
	^ self materialize header
]

{ #category : 'running' }
FLMaterializer >> materializeObjectCollection [
	map
		at: #hasMultipleObjects
		put: true.
	
	^ self materialize
]

{ #category : 'running' }
FLMaterializer >> materializeRoot [
	^ self materialize root
]

{ #category : 'configuring-migrations' }
FLMaterializer >> migrateClassNamed: aSymbol toClass: aClassOrSymbol [
	"Use aClassOrSymbol instead of aSymbol after materialization. This only changes the class name.
	The target class is expected to exist in the target system's global environment.
	Variables are expected to be unchanged. Extra variables will be nil, missing variables will
	be ignored.
	Example (change name of class #MyPoint to #YourPoint):
		configuration
			migrateClassNamed: #MyPoint
			toClass: #YourPoint }"

	self
		migrateClassNamed: aSymbol
		toClass: aClassOrSymbol
		variables: Dictionary new
]

{ #category : 'configuring-migrations' }
FLMaterializer >> migrateClassNamed: aSymbol toClass: aClassOrSymbol variables: aDictionary [
	"Use aClassOrSymbol instead of aSymbol after materialization and migrate variable names as defined in 
	aDictionary. The new class is expected to exist in the target system's global environment.
	Variables are expected to be unchanged with exception to those in aDictionary. Extra variables will 
	be nil, missing variables will be ignored.
	Example (change class name from #MyPoint to #YourPoint and change name of variable 'x' to 'xCoordinate'):
		configuration
			migrateClassNamed: #MyPoint
			toClass: #YourPoint
			variables: { x -> xCoordinate }"

	| class |
	self
		validateMigrationArgumentsFrom: aSymbol
		to: aClassOrSymbol
		variables: aDictionary.
	
	class := aClassOrSymbol isString
		ifTrue: [
			self environment
				at: aClassOrSymbol asSymbol
				ifPresent: [ :global |
					global isClass
						ifTrue: [ global ]
						ifFalse: [
							FLConfigurationError signal: 'Target global must be a class' ] ]
				ifAbsent: [
					FLConfigurationError signal: 'Global not found: ', aClassOrSymbol ] ]
		ifFalse: [ aClassOrSymbol ].
	
	self
		basicMigrateClassNamed: aSymbol asSymbol
		toClass: class
		variables: aDictionary
]

{ #category : 'configuring-migrations' }
FLMaterializer >> migrateClassNamed: aSymbol variables: aDictionary [
	"Migrates variable names as defined in aDictionary.
	Variables are expected to be unchanged with exception to those in aDictionary. Extra variables will 
	be nil, missing variables will be ignored.
	Example (change name of variable 'x' to 'xCoordinate'):
		configuration
			migrateClassNamed: #MyPoint
			variables: { x -> xCoordinate }"
	
	self
		migrateClassNamed: aSymbol
		toClass: aSymbol
		variables: aDictionary
]

{ #category : 'accessing' }
FLMaterializer >> migrations [
	^ map
		at: #migrations
		ifAbsentPut: [ OrderedCollection new ]
]

{ #category : 'testing' }
FLMaterializer >> shouldMaterializeHeaderOnly [
	^ map
		at: #shouldMaterializeHeaderOnly
		ifAbsent: [ self class defaultShouldMaterializeHeaderOnly ]
]

{ #category : 'private' }
FLMaterializer >> streamFactoryForFilePath: aString [
	^ [
	  (File named: aString) readStream
		  binary;
		  yourself ]
]

{ #category : 'private' }
FLMaterializer >> validateMigrationArgumentsFrom: aSymbol to: aClassOrSymbol variables: aDictionary [
	aSymbol isString ifFalse: [
		FLConfigurationError signal: 'Source must be a symbol' ].
	
	(aClassOrSymbol isString or: [
		aClassOrSymbol isClass ]) ifFalse: [
			FLConfigurationError signal: 'Target must be a class or symbol' ].
		
	aDictionary isDictionary ifFalse: [
		FLConfigurationError signal: 'Variable map must be a dictionary' ]
]

{ #category : 'private' }
FLMaterializer >> validateMigrationFrom: aSymbol to: aClass [
	aSymbol isString ifFalse: [
		FLConfigurationError signal: 'Source class name must be a symbol' ].
		
	self migrations
		detect: [ :migration | migration sourceClassName = aSymbol ]
		ifNone: [ ^ self ].
		
	FLConfigurationError signal: 'Existing migration for ', aSymbol, ' found.',
		' You can''t register more than one migration for every source class'
]
