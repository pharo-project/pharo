"
I am the configuration for serialization.

I hold all the configuration information necessary for serializing objects.
"
Class {
	#name : 'FLSerializer',
	#superclass : 'FLConfiguration',
	#category : 'Fuel-Core-Public',
	#package : 'Fuel-Core',
	#tag : 'Public'
}

{ #category : 'accessing-defaults' }
FLSerializer class >> defaultDepthLimit [
	^ -1
]

{ #category : 'accessing-defaults' }
FLSerializer class >> defaultLimitObjectsExceptionEnabled [
	^ false
]

{ #category : 'accessing-defaults' }
FLSerializer class >> defaultObjectsLimit [
	^ -1
]

{ #category : 'convenience' }
FLSerializer class >> serialize: anObject on: aStream [
	self new
		object: anObject;
		onStream: aStream;
		serialize
]

{ #category : 'convenience' }
FLSerializer class >> serialize: anObject toFileNamed: aFilePath [
	self new
		object: anObject;
		filePath: aFilePath;
		serialize
]

{ #category : 'convenience' }
FLSerializer class >> serializeToByteArray: anObject [
	^ ByteArray
		new: 100
		streamContents: [ :stream |
			self new
				object: anObject;
				onStream: stream;
				serialize ]
]

{ #category : 'configuring-materialization' }
FLSerializer >> addPostMaterializationAction: aCleanBlockClosure [
	"Takes a clean (no dependency on outer variables) block closure which will be
	activated after materialization automatically.
	This closure takes three optional arguments:
		1. the header (FLHeader)
		2. the materialization (FLMateralization)
		3. the global environment
	
	Example: 
	
	configuration addPostMaterializationAction: [ :header :materialization :environment |
		| selector |
		selector := header additionalObjectAt: #selector.
		materialization root perform: 'Materialization done!' ]"
	aCleanBlockClosure isBlock ifFalse: [
		FLConfigurationError signal: 'Argument must be a block' ].
	
	aCleanBlockClosure isClean ifFalse: [
		FLConfigurationError signal: 'Post materializaton actions have to be clean closures. For more details see method BlocKClosure >> #isClean' ].
	
	aCleanBlockClosure numArgs > 3 ifTrue: [
		FLConfigurationError signal: 'Block accepts too many arguments, only three will be passed' ].
	
	(map
		at: #postMaterializationActions
		ifAbsentPut: [ IdentitySet new ])
			add: aCleanBlockClosure
]

{ #category : 'configuring-materialization' }
FLSerializer >> addPreMaterializationAction: aCleanBlockClosure [
	"Takes a clean (no dependency on outer variables) block closure which will be
	activated before materialization automatically.
	This closure takes two optional arguments:
		1. the header (FLHeader)
		2. the global environment
	
	Example:
	
	configuration addPostMaterializationAction: [ :header :environment |
		environment at: (header additionalObjectAt: #transcripClassName)
			show: 'Materialization starting!' ]"
	aCleanBlockClosure isBlock ifFalse: [
		FLConfigurationError signal: 'Argument must be a block' ].
	
	aCleanBlockClosure isClean ifFalse: [
		FLConfigurationError signal: 'Pre materializaton actions have to be clean closures. For more details see method BlocKClosure >> #isClean' ].
	
	aCleanBlockClosure numArgs > 2 ifTrue: [
		FLConfigurationError signal: 'Block accepts too many arguments, only two will be passed' ].
	
	(map
		at: #preMaterializationActions
		ifAbsentPut: [ IdentitySet new ])
			add: aCleanBlockClosure
]

{ #category : 'configuring-materialization' }
FLSerializer >> at: aKey putAdditionalObject: anObject [ 
	"This is useful if we want to attach objects to a package that will also be serialized. The way they are stored is key-value."
	(map
		at: #additionalObjects
		ifAbsentPut: [ IdentityDictionary new ])
			at: aKey
			put: anObject
]

{ #category : 'accessing' }
FLSerializer >> behaviorsToSerialize [
	^ map
		at: #behaviorsToSerialize
		ifAbsentPut: [ IdentitySet new ]
]

{ #category : 'private' }
FLSerializer >> computeGlobals [
	| globals defaultGlobals configuredGlobals allGlobals |
	defaultGlobals := (self class defaultGlobalSymbols select: [ :symbol |
		self environment includesKey: symbol ]) asSet.
	configuredGlobals := map
		at: #globals
		ifAbsent: [ #() ].
	allGlobals := defaultGlobals, configuredGlobals.
	allGlobals isEmpty ifTrue: [ ^ IdentityDictionary new ].
	
	globals := IdentityDictionary new.
	allGlobals do: [ :object |
		globals add: (self globalAssociationFor: object) ].
	
	^ globals
]

{ #category : 'accessing' }
FLSerializer >> depthLimit [
	^ map
		at: #depthLimit
		ifAbsent: [ self class defaultDepthLimit ]
]

{ #category : 'configuring' }
FLSerializer >> enableLimitObjectsException [
	"Setting this to true will force Fuel to signal an exception if the number of objects
	that will be serialized exceeds the limit set via #limitObjectsTo:"

	map
		at: #limitObjectsExceptionEnabled
		put: true
]

{ #category : 'configuring-globals' }
FLSerializer >> fullySerializeAllBehaviors: aCollection [
	aCollection isCollection ifFalse: [
		FLConfigurationError signal: 'Argument must be a collection' ].
	
	aCollection do: [ :behavior |
		self fullySerializeBehavior: behavior ]
]

{ #category : 'configuring-globals' }
FLSerializer >> fullySerializeAllMethods: aCollection [
	aCollection isCollection ifFalse: [
		FLConfigurationError signal: 'Argument must be a collection' ].
	
	aCollection do: [ :behavior |
		self fullySerializeMethod: behavior ]
]

{ #category : 'configuring-globals' }
FLSerializer >> fullySerializeBehavior: aBehavior [
	aBehavior isBehavior ifFalse: [
		FLConfigurationError signal: 'Argument must be a Behavior' ].
	
	self behaviorsToSerialize add: aBehavior
]

{ #category : 'configuring-globals' }
FLSerializer >> fullySerializeMethod: aCompiledMethod [
	aCompiledMethod isCompiledMethod ifFalse: [
		FLConfigurationError signal: 'Argument must be a CompiledMethod' ].
	
	self methodsToSerialize add: aCompiledMethod
]

{ #category : 'private' }
FLSerializer >> globalAssociationFor: anObject [
	"Some globals, like ActiveHand have a nil value in the Smalltalk globals. Therefore, we cannot map nil to globalCluster. We could filter before in #defaultGlobalSymbols but that means that not even the Association will be consider global."
	(self environment
		at: anObject
		ifPresent: [ :value |
			^ anObject -> (value ifNil: [ anObject ]) ]
		ifAbsent: [
			self environment
				keyAtIdentityValue: anObject
				ifPresent: [ :key |
					^ key -> anObject ]
				ifAbsent: [
					FLConfigurationError signal: 'No such global found in environment: ', anObject asString ] ])
]

{ #category : 'accessing' }
FLSerializer >> globals [
	^ map
		at: #computedGlobals
		ifAbsentPut: [ self computeGlobals ]
]

{ #category : 'testing' }
FLSerializer >> hasDepthLimit [
	^ self depthLimit > 0
]

{ #category : 'testing' }
FLSerializer >> hasObjectsLimit [
	^ self objectLimit > 0
]

{ #category : 'testing' }
FLSerializer >> isLimitObjectsExceptionEnabled [
	^ map
		at: #limitObjectsExceptionEnabled
		ifAbsent: [ self class defaultLimitObjectsExceptionEnabled ]
]

{ #category : 'configuring' }
FLSerializer >> limitDepthTo: anInteger [
	"Fuel will traverse the serialization object graph until it has reached the depth
	specified and ignore any objects above the limit. You can think of the depth as the distance
	between the root object and the one currently being analyzed.
	
	You should be aware that this can have unexpected side effects since it his hard to tell what
	objects will be missing in the resulting object graph.
	
	Graph traversal is depth first."
	
	anInteger isInteger ifFalse: [
		FLConfigurationError signal: 'Depth limit must be an integer' ].

	map
		at: #depthLimit
		put: anInteger
]

{ #category : 'configuring' }
FLSerializer >> limitObjectsTo: anInteger [
	"Fuel will traverse the serialization object graph until it has reached the number of objects
	specified. Depending on the setting of #enableLimitObjectsException Fuel will then either
	signal an exception or continue with writing out the encountered objects.
	
	You should be aware that this can have unexpected side effects since it his hard to tell what
	objects will be missing in the resulting object graph.
	
	Graph traversal is depth first."

	anInteger isInteger ifFalse: [
		FLConfigurationError signal: 'Object limit must be an integer' ].
	
	map
		at: #objectLimit
		put: anInteger
]

{ #category : 'configuring-globals' }
FLSerializer >> lookUpAllInGlobals: aCollection [
	aCollection isCollection ifFalse: [
		FLConfigurationError signal: 'Argument must be collection' ].
	
	aCollection do: [ :object |
		self lookUpInGlobals: object ]
]

{ #category : 'configuring-globals' }
FLSerializer >> lookUpInGlobals: anObject [
	(map
		at: #globals
		ifAbsentPut: [ IdentitySet new ])
			add: anObject
]

{ #category : 'accessing' }
FLSerializer >> methodsToSerialize [
	^ map
		at: #methodsToSerialize
		ifAbsentPut: [ IdentitySet new ]
]

{ #category : 'configuring' }
FLSerializer >> object: anObject [
	map
		at: #object
		ifPresent: [ :object | FLConfigurationError signal: 'Object already set' ]
		ifAbsentPut: [ anObject ]
]

{ #category : 'configuring' }
FLSerializer >> objectCollection: aCollection [
	aCollection isCollection ifFalse: [
		FLConfigurationError signal: 'Argument must be a collection' ].
	
	self object: aCollection.
	
	map
		at: #hasMultipleObjects
		put: true
]

{ #category : 'accessing' }
FLSerializer >> objectLimit [
	^ map
		at: #objectLimit
		ifAbsent: [ self class defaultObjectsLimit ]
]

{ #category : 'accessing' }
FLSerializer >> postMaterializationActions [
	^ map
		at: #postMaterializationActions
		ifAbsent: [ IdentityDictionary new ]
]

{ #category : 'accessing' }
FLSerializer >> preMaterializationActions [
	^ map
		at: #preMaterializationActions
		ifAbsent: [ IdentityDictionary new ]
]

{ #category : 'running' }
FLSerializer >> serialize [
	self useDuring: [ FLSerialization run ]
]

{ #category : 'private' }
FLSerializer >> streamFactoryForFilePath: aString [
	^ [
	  (File named: aString) writeStream
		  binary;
		  yourself ]
]

{ #category : 'accessing' }
FLSerializer >> substitutions [
	^ map
		at: #substitutions
		ifAbsent: [ Bag new ]
]

{ #category : 'configuring' }
FLSerializer >> when: aConditionBlock substituteBy: aSubstitutionBlock [
	"Dynamically substitute any object in the object graph with aValuable (understands #value and #value:)
	if conditionBlock answers true.
	Every object in the graph will be passed as an argument to aConditionBlock and to aValuable if
	aCondition Block answers true.
	Note that this may reduce performance significantly compared to static substitution (i.e. using methods
	on the classes of the objects to replace, e.g. #fuelReplacement).
	Example:
		configuration
			when: [ :object | object isString ]
			substituteBy: [ :object | nil ]"
	aConditionBlock isBlock ifFalse: [
		FLConfigurationError signal: 'Condition must be a block' ].
	aConditionBlock numArgs = 1 ifFalse: [
		FLConfigurationError signal: 'Condition block must accept the currently analysed object as single argument' ].
	aSubstitutionBlock isBlock ifFalse: [
		FLConfigurationError signal: 'Substitution must be a block' ].
	aSubstitutionBlock numArgs = 1 ifFalse: [
		FLConfigurationError signal: 'Substitution block must accept the currently analysed object as single argument' ].

	(map
		at: #substitutions
		ifAbsentPut: [ Bag new ])
			add: aConditionBlock -> aSubstitutionBlock
]
