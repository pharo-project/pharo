"
An RGBehaviorDefinition is an abstract definition for class-alike entities (e.g. classes, traits)

Instance Variables
	methods:		<Collection>
	protocols:		<Collection>
	superclass:		<Object>
"
Class {
	#name : #RGBehaviorDefinition,
	#superclass : #RGGlobalDefinition,
	#instVars : [
		'superclass',
		'methods',
		'protocols'
	],
	#category : #'Ring-Deprecated-Core-Kernel'
}

{ #category : #'class-annotations' }
RGBehaviorDefinition class >> allSubclassesKey [

	^#allSubclasses
]

{ #category : #'class-annotations' }
RGBehaviorDefinition class >> allSuperclassesKey [

	^#allSuperclasses
]

{ #category : #'class-annotations' }
RGBehaviorDefinition class >> definitionSourceKey [

	^#definitionSource
]

{ #category : #'class-annotations' }
RGBehaviorDefinition class >> isPoolKey [

	^#isPool
]

{ #category : #'class-annotations' }
RGBehaviorDefinition class >> subclassesKey [

	^#subclasses
]

{ #category : #'class-annotations' }
RGBehaviorDefinition class >> superclassNameKey [
	
	^#superclassName
]

{ #category : #'class-annotations' }
RGBehaviorDefinition class >> superclassesKey [

	^#superclasses
]

{ #category : #'class-annotations' }
RGBehaviorDefinition class >> traitCompositionSourceKey [

	^#traitCompositionSource
]

{ #category : #'class-annotations' }
RGBehaviorDefinition class >> usersKey [

	^#users
]

{ #category : #operations }
RGBehaviorDefinition >> addCategory: newName before: aCategory [

	^ self addProtocol: newName
]

{ #category : #'managing container' }
RGBehaviorDefinition >> addInContainer: aRGContainer [

	aRGContainer addClass: self
]

{ #category : #'adding/removing methods' }
RGBehaviorDefinition >> addMethod: aRGMethodDefinition [
	"Adds aRGMethodDefinition in the methods dictionary.
	 Adds the protocol of such method too"

	self addMethod: aRGMethodDefinition in: methods.
	self addProtocol: aRGMethodDefinition protocol
]

{ #category : #private }
RGBehaviorDefinition >> addMethod: aRGMethodDefinition in: aCollection [
	"Adds aRGMethodDefinition in the collection received"

	aRGMethodDefinition parent ifNil:[ aRGMethodDefinition parent: self ].
	aCollection at: aRGMethodDefinition selector
		put: aRGMethodDefinition
]

{ #category : #'adding/removing protocols' }
RGBehaviorDefinition >> addProtocol: anObject [
	"Adds a protocol named anObject.
	Protocols are not repeated"
	
	anObject ifNil:[ ^self ].
	protocols add: anObject
]

{ #category : #'adding/removing methods' }
RGBehaviorDefinition >> addSelector: selectorName classified: protocolName sourced: source [

	self addMethod: ((RGMethodDefinition named: selectorName) 
							parent: self;
							protocol: protocolName;
							sourceCode: source;
							yourself)
]

{ #category : #subclassing }
RGBehaviorDefinition >> addSubclass: aRGBehaviorDefinition [
	"Adds a direct subclass of the receiver"

	(self subclasses includes: aRGBehaviorDefinition)
		ifFalse:[ self subclasses add: aRGBehaviorDefinition ]
]

{ #category : #'accessing methods' }
RGBehaviorDefinition >> allSelectors [
	"Retrieves all the selectos of the receiver in the chain hierarchy"
	| class selectors |
	
	class:= self.
	selectors := Set new.
	[class notNil] whileTrue: 
			[selectors addAll: class selectors.
			class := class superclass ].
	^selectors
]

{ #category : #'accessing class hierarchy' }
RGBehaviorDefinition >> allSubclasses [
	"Retrieves all the subclasses of the receiver in the chan hiearchy - value is kept as an annotation"
	"is a good idea storing this?"
	
	^self annotationNamed: self class allSubclassesKey
		ifAbsentPut:[ | collection index |
			index := 1.
			collection := OrderedCollection withAll: self subclasses.
			[index <= collection size] whileTrue: 
				[collection addAll: (collection at: index) subclasses.
				index := index + 1].
			collection ]
]

{ #category : #'accessing class hierarchy' }
RGBehaviorDefinition >> allSubclasses: aCollection [
	"Stores all the subclasses (direct and indirect) as an annotation"
	
	self annotationNamed: self class allSubclassesKey
		ifAbsentPut:[ aCollection ]
]

{ #category : #'accessing class hierarchy' }
RGBehaviorDefinition >> allSuperclasses [
	"Answer an OrderedCollection of the receiver's and the receiver's  
	ancestor's superclasses"
	"Is implementation of Behavior more efficient?"
	
	^self annotationNamed: self class allSuperclassesKey
		ifAbsentPut:[ | supers sprClass |
			supers := OrderedCollection new.
			sprClass := self superclass.
			[sprClass notNil] whileTrue: 
				[supers add: sprClass.
				sprClass := sprClass superclass].
			supers ]
]

{ #category : #'accessing class hierarchy' }
RGBehaviorDefinition >> allSuperclasses: aCollection [
	"Stores all the superclasses (direct and indirect) as an annotation"
	
	self annotationNamed: self class allSuperclassesKey
		ifAbsentPut:[ aCollection ]
]

{ #category : #'accessing class hierarchy' }
RGBehaviorDefinition >> allSuperclassesDo: aBlock [
	"Evaluate the argument, aBlock, for each of the receiver's superclasses."

	self superclass isNil
		ifTrue: [ ^ self ].
	aBlock value: superclass.
	superclass allSuperclassesDo: aBlock
]

{ #category : #'accessing - parallel hierarchy' }
RGBehaviorDefinition >> classSide [
	"Return the metaclass of the couple class/metaclass. Useful to avoid explicit test."
	
	^ self subclassResponsibility
]

{ #category : #'accessing methods' }
RGBehaviorDefinition >> compiledMethodNamed: selector [
	"Retrieves the compiled method from aRGMethodDefinition"
	| method |

	^(method:= self methodNamed: selector) notNil
	ifTrue: [ method compiledMethod ]
	ifFalse:[ nil ]
]

{ #category : #'filein/out' }
RGBehaviorDefinition >> definition [

	^ self definitionSource
]

{ #category : #annotations }
RGBehaviorDefinition >> definitionSource [
	"Retrieves the definition template of the receiver -> aString.
	This value is kept as an annotation"
	
	^self annotationNamed: self class definitionSourceKey
]

{ #category : #annotations }
RGBehaviorDefinition >> definitionSource: aString [
	"Sets the definition template of the receiver -> aString.
	It's stored as an annotation"
	
	self annotationNamed: self class definitionSourceKey
		put: aString
]

{ #category : #'accessing methods' }
RGBehaviorDefinition >> extensionMethods [

	^self methods select:[ :each | each isExtension ]
]

{ #category : #annotations }
RGBehaviorDefinition >> hasDefinitionSource [
	"Tests whether the receiver has a source definition."
	
	^ self definitionSource notNil
]

{ #category : #testing }
RGBehaviorDefinition >> hasMetaclass [

	^ false
]

{ #category : #testing }
RGBehaviorDefinition >> hasMethods [
	"validates the existance of methods"

	^methods notEmpty
]

{ #category : #testing }
RGBehaviorDefinition >> hasProtocols [
	"Validates the existance of protocols"

	^protocols notEmpty
]

{ #category : #testing }
RGBehaviorDefinition >> hasSuperclass [

	^superclass notNil
]

{ #category : #testing }
RGBehaviorDefinition >> hasTraitComposition [

	^self traitCompositionSource ~= '{}'
]

{ #category : #'testing class hierarchy' }
RGBehaviorDefinition >> includesBehavior: aClass [
	^self == aClass or: [self inheritsFrom: aClass]
]

{ #category : #testing }
RGBehaviorDefinition >> includesProtocol: aString [
	    "Looks for a protocols named = aString"
    
	    ^protocols includes: aString
]

{ #category : #'accessing methods' }
RGBehaviorDefinition >> includesSelector: selector [
	"Looks if selector is a key in the methods dictionary"
	
	^methods includesKey: selector asSymbol
]

{ #category : #initialization }
RGBehaviorDefinition >> initialize [

	super initialize.
	methods:= IdentityDictionary new.
	protocols:= Set new.
]

{ #category : #'accessing - parallel hierarchy' }
RGBehaviorDefinition >> instanceSide [
	"Return the class of the couple class/metaclass. Useful to avoid explicit test."
	
	^ self subclassResponsibility
]

{ #category : #testing }
RGBehaviorDefinition >> isDefined [
	"If the class exists in the environment"
	
	^self realClass notNil
	
]

{ #category : #'managing container' }
RGBehaviorDefinition >> isIncludedInContainer: aRGContainer [

	^aRGContainer includesClass: self
]

{ #category : #testing }
RGBehaviorDefinition >> isMeta [
	"By default is considered a non-meta class"

	^false
]

{ #category : #testing }
RGBehaviorDefinition >> isSameRevisionAs: aRGBehaviorDefinition [
	"This method look for equality of the properties of the receiver"

	^self class = aRGBehaviorDefinition class
		and:[ self name == aRGBehaviorDefinition name ]
]

{ #category : #'accessing class hierarchy' }
RGBehaviorDefinition >> methodDict [
	^ methods
]

{ #category : #'accessing methods' }
RGBehaviorDefinition >> methodNamed: selector [
	"Retrieves aRGMethodDefinition that matches the selector given as argument"
	"RB defines methodFor:"

	^methods at: selector asSymbol ifAbsent:[ nil ]
]

{ #category : #accessing }
RGBehaviorDefinition >> methods [

	^methods
]

{ #category : #accessing }
RGBehaviorDefinition >> methods: aDictionary [

	methods:= aDictionary
]

{ #category : #'accessing methods' }
RGBehaviorDefinition >> methodsInProtocol: aString [
	"Retrieves the methods classified in protocol named aString"

	^methods select: [ :each | each protocol = aString ]
]

{ #category : #accessing }
RGBehaviorDefinition >> package [
	
	self subclassResponsibility
]

{ #category : #printing }
RGBehaviorDefinition >> printOn: aStream [ 
	aStream nextPutAll: self name
]

{ #category : #accessing }
RGBehaviorDefinition >> protocols [
	"retrieves the protocols of the class"

	^protocols
]

{ #category : #accessing }
RGBehaviorDefinition >> protocols: aCollection [
	"set the protocols of the class"

	protocols:= aCollection
]

{ #category : #accessing }
RGBehaviorDefinition >> realClass [
	"Retrieves the current class existing in the runtime environment"

	^self rootEnvironment classNamed: self name 
]

{ #category : #'managing container' }
RGBehaviorDefinition >> removeFromContainer: aRGContainer [

	aRGContainer removeClass: self
]

{ #category : #'adding/removing methods' }
RGBehaviorDefinition >> removeMethod: aRGMethodDefinition [
	"Removes aRGMethodDefinition from the methods dictionary"

	self removeMethod: aRGMethodDefinition from: methods
]

{ #category : #private }
RGBehaviorDefinition >> removeMethod: aRGMethodDefinition from: aCollection [
	"Removes aRGMethodDefinition from the collection received"

	aCollection removeKey: aRGMethodDefinition selector ifAbsent:[]
]

{ #category : #'adding/removing protocols' }
RGBehaviorDefinition >> removeProtocol: aString [
	"Removes a protocol named aString (if exists)"

	protocols remove: aString ifAbsent:[]
]

{ #category : #'adding/removing methods' }
RGBehaviorDefinition >> removeSelector: selector [
	"Removes a method named as selector"

	methods removeKey: selector ifAbsent:[]
]

{ #category : #subclassing }
RGBehaviorDefinition >> removeSubclass: aRGBehaviorDefinition [ 
	"Removes aRGAbstractClassDefinition from the direct subclasses - without failing if does not exist"
	
	self subclasses remove: aRGBehaviorDefinition ifAbsent: []
]

{ #category : #'accessing methods' }
RGBehaviorDefinition >> selectors [
	"Retrieves the method selectors"

	^methods keys
]

{ #category : #compatibility }
RGBehaviorDefinition >> soleInstance [
	"to be depracated in the future"
	
	^self instanceSide
]

{ #category : #printing }
RGBehaviorDefinition >> storeOn: aStream [ 
	self name storeOn: aStream
]

{ #category : #annotations }
RGBehaviorDefinition >> subclasses [
	"Retrieves the direct subclasses of the receiver.
	This value is kept as an annotation"

	^self annotationNamed: self class subclassesKey
		ifAbsentPut:[ OrderedCollection new ]
]

{ #category : #annotations }
RGBehaviorDefinition >> subclasses: aCollection [
	"Stores direct subclasses of the receiver as an annotation"

	self annotationNamed: self class subclassesKey
		ifAbsentPut:[ aCollection ]
]

{ #category : #accessing }
RGBehaviorDefinition >> superclass [

	^superclass
]

{ #category : #accessing }
RGBehaviorDefinition >> superclass: aRGBehaviorDefinition [
	"The superclass is assigned.
	If aRGBehaviorDefinition is not nil the receiver is added as a subclass and the superclass assignment also happens for theMetaClass"

	superclass := aRGBehaviorDefinition.
	superclass notNil
		ifFalse: [ ^ self ].
	self superclassName: aRGBehaviorDefinition name.
	aRGBehaviorDefinition addSubclass: self.
	self hasMetaclass
		ifTrue: [ self classSide superclass: aRGBehaviorDefinition classSide ]
]

{ #category : #annotations }
RGBehaviorDefinition >> superclassName [
	"Retrieves the name of the superclass if exists"

	^self annotationNamed: self class superclassNameKey
]

{ #category : #annotations }
RGBehaviorDefinition >> superclassName: aSymbol [

	self annotationNamed: self class superclassNameKey 
		put: aSymbol
]

{ #category : #'accessing - deprecated parallel hierarchy' }
RGBehaviorDefinition >> theMetaClass [

	self subclassResponsibility
]

{ #category : #'accessing - deprecated parallel hierarchy' }
RGBehaviorDefinition >> theNonMetaClass [

	self subclassResponsibility
]

{ #category : #annotations }
RGBehaviorDefinition >> traitCompositionSource [
	"Retrieves aString representing the used traits"

	^self annotationNamed: self class traitCompositionSourceKey
		ifAbsentPut:[  '{}'  ]
]

{ #category : #annotations }
RGBehaviorDefinition >> traitCompositionSource: anString [
	"Stores aString representing the traits used by the receiver "

	self annotationNamed: self class traitCompositionSourceKey
		put: anString 
]

{ #category : #accessing }
RGBehaviorDefinition >> traitNames [
	"Assuming that traits in a composition can be identified by
	testing for the first character being an uppercase character
	(and thus not a special character such as {, # etc.)"
	| tokens |
	
	tokens := self traitCompositionSource parseLiterals flattened.
	^tokens select: [:each | each first isUppercase].
]

{ #category : #accessing }
RGBehaviorDefinition >> traits [
	"Retrieves ring traits based on the names in the traitComposition and from the environment if it is a ring object"
	
	^ self environment isRingObject 	
		ifTrue: [ self traitNames collect:[ :each| self environment traitNamed: each ] ]
		ifFalse:[ #() ]
]

{ #category : #'accessing class hierarchy' }
RGBehaviorDefinition >> withAllSubclasses [
	"if allSubclasses is stored should not affect the collection"

	^self allSubclasses, {self}
]

{ #category : #'accessing class hierarchy' }
RGBehaviorDefinition >> withAllSuperclasses [
	"if allSuperclasses is stored should not affect the collection"

	^self allSuperclasses, {self}
]
