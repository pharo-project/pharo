"
RGClassDefinition is the concrete representation of a class (no trait)
"
Class {
	#name : #RGClassDefinition,
	#superclass : #RGClassDescriptionDefinition,
	#instVars : [
		'metaClass',
		'comment',
		'classVariables',
		'category',
		'package',
		'sharedPools'
	],
	#category : #'Ring-Deprecated-Core-Kernel'
}

{ #category : #'instance creation' }
RGClassDefinition class >> createSharedPoolNamed: aSymbol [
	"A shared pool is a class inheriting from #SharedPool"

	^(RGClassDefinition named: aSymbol)
			superclassName: #SharedPool;
			isPool: true;
			yourself
]

{ #category : #'class variables' }
RGClassDefinition >> addClassVarNamed: aString [
	| var |
	var := (RGClassVariableDefinition named: aString) parent: self.
	self addVariable: var in: classVariables.
	^var
]

{ #category : #'class variables' }
RGClassDefinition >> addClassVariable: aRCClassVariable [

	self addVariable: (aRCClassVariable parent: self) 
		in: classVariables
]

{ #category : #'class variables' }
RGClassDefinition >> addClassVariables: aCollection [
	
	aCollection do: [:var | self addClassVarNamed: var ]
]

{ #category : #'shared pools' }
RGClassDefinition >> addSharedPool: aRGPoolVariableDefinition [
	self 
		addVariable: (aRGPoolVariableDefinition parent: self) 
		in: sharedPools
]

{ #category : #'shared pools' }
RGClassDefinition >> addSharedPoolNamed: poolName [
	| pool |
	pool:= (RGPoolVariableDefinition named:  poolName) parent: self.
	self addVariable: pool in: sharedPools.
	^pool
]

{ #category : #'shared pools' }
RGClassDefinition >> addSharedPools: aCollection [
	aCollection do: [ :pool | self addSharedPoolNamed: pool ]
]

{ #category : #'managing pool users' }
RGClassDefinition >> addUser: aRGClassDefinition [
	"The receiver registers the aRGClassDefinition as an user.  An reinforces its status as a shared pool."
	
	aRGClassDefinition isClass ifFalse:[ ^self ].
	(aRGClassDefinition instanceSide sharedPoolNamed: self name) isNil
		ifTrue: [ aRGClassDefinition instanceSide addSharedPoolNamed: self name ].
	self isPool: true.
	self users add: aRGClassDefinition instanceSide
]

{ #category : #'class variables' }
RGClassDefinition >> allClassVarNames [

	^self allClassVariables collect:[ :cvar| cvar name ]
]

{ #category : #'class variables' }
RGClassDefinition >> allClassVariables [
	"Answer a collection of the receiver's classVariables, including those defined its superclasses"

	^self hasSuperclass 
		ifFalse:[ classVariables ]
		ifTrue:[ self superclass allClassVariables, classVariables ]
]

{ #category : #'shared pools' }
RGClassDefinition >> allSharedPoolNames [
	^ self allSharedPools collect: [ :pool | pool name ]
]

{ #category : #'shared pools' }
RGClassDefinition >> allSharedPools [
	"Answer a collection of the pools the receiver shares, including those defined  
	in the superclasses of the receiver."

	^ self hasSuperclass
		ifFalse: [ sharedPools ]
		ifTrue: [ self superclass allSharedPools , sharedPools ]
]

{ #category : #accessing }
RGClassDefinition >> category [
	"retrieves a tag for its package"
	
	^category
]

{ #category : #accessing }
RGClassDefinition >> category: aSymbol [
	"stores a tag for its package"

	category := aSymbol
]

{ #category : #'accessing - parallel hierarchy' }
RGClassDefinition >> classSide [
	"Return the metaclass of the couple class/metaclass. Useful to avoid explicit test."
	^ metaClass
]

{ #category : #'class variables' }
RGClassDefinition >> classVarNamed: aString [

	^classVariables detect:[ :v| v name = aString asSymbol ] ifNone:[ nil ]
]

{ #category : #'class variables' }
RGClassDefinition >> classVarNames [
	"Answer a collection of the names of the class variables defined in the receiver."
	
	^classVariables collect:[ :cvar| cvar name ]
]

{ #category : #accessing }
RGClassDefinition >> classVariables [

	^classVariables
]

{ #category : #accessing }
RGClassDefinition >> classVariables: aCollection [

	classVariables:= aCollection
]

{ #category : #accessing }
RGClassDefinition >> comment [
	"Retrieves the comment definition object"

	^comment
]

{ #category : #accessing }
RGClassDefinition >> comment: anObject [
	"Sets a RGCommentDefinition object based on the argument"
	
	comment := anObject isRingObject 
		ifFalse: [ 
			RGCommentDefinition new
				parent: self;
				content: anObject; 
				yourself ]
		ifTrue: [anObject ]
	
	 
]

{ #category : #compiling }
RGClassDefinition >> compile: aString classified: aCategory notifying: aController [

	self realClass compile: aString classified: aCategory notifying: aController
]

{ #category : #testing }
RGClassDefinition >> hasComment [

	^comment isEmptyOrNil not
]

{ #category : #testing }
RGClassDefinition >> hasMetaclass [

	^metaClass notNil
]

{ #category : #testing }
RGClassDefinition >> hasStamp [

	^self stamp isEmptyOrNil not
]

{ #category : #'managing pool users' }
RGClassDefinition >> includesUser: aRGBehaviorDefinition [

	^self users includes: aRGBehaviorDefinition
]

{ #category : #accessing }
RGClassDefinition >> inheritsFrom: aClass [

	^ self realClass inheritsFrom: aClass
]

{ #category : #initialization }
RGClassDefinition >> initialize [

	super initialize.
	classVariables:= OrderedCollection new.
	sharedPools:= OrderedCollection new.
]

{ #category : #'accessing - parallel hierarchy' }
RGClassDefinition >> instanceSide [
	"Return the class of the couple class/metaclass. Useful to avoid explicit test."
	^ self
]

{ #category : #'managing pool users' }
RGClassDefinition >> isPool [
	"The receiver is a shared pool if it inherits from SharedPool"
	
	^self annotationNamed: self class isPoolKey 
		ifAbsent: [ self superclassName == #SharedPool ]
]

{ #category : #'managing pool users' }
RGClassDefinition >> isPool: aBoolean [
	
	^self annotationNamed: self class isPoolKey put: aBoolean
]

{ #category : #testing }
RGClassDefinition >> isSameRevisionAs: aRGClassDefinition [
	"This method look for equality of the properties of the receiver"

	^(super isSameRevisionAs: aRGClassDefinition)
		and:[ self superclassName == aRGClassDefinition superclassName
		and:[ self category = aRGClassDefinition category 
		and:[ self classVarNames sorted = aRGClassDefinition classVarNames sorted 
		and:[ self sharedPoolNames sorted = aRGClassDefinition sharedPoolNames sorted  
		and:[ self traitCompositionSource = aRGClassDefinition traitCompositionSource 
		and:[ ((self hasComment and:[ self comment isSameRevisionAs: aRGClassDefinition comment ]) or:[ self hasComment not ])
		and:[ (self classSide isSameRevisionAs: aRGClassDefinition classSide) ] ] ] ] ] ] ]
]

{ #category : #accessing }
RGClassDefinition >> package [
	"Retrieves the package in which this class is contained, if exists"
	
	^package
]

{ #category : #accessing }
RGClassDefinition >> package: aRGPackage [
	"Sets the package in which this class is contained"
	
	package:= aRGPackage
]

{ #category : #'class variables' }
RGClassDefinition >> removeClassVarNamed: aString [

	self removeVariable: (self classVarNamed: aString) from: classVariables
]

{ #category : #'class variables' }
RGClassDefinition >> removeClassVariable: aRGClassVariableDefinition [

	self removeVariable: aRGClassVariableDefinition from: classVariables
]

{ #category : #'shared pools' }
RGClassDefinition >> removeSharedPool: aRGPoolVariableDefinition [

	self removeVariable: aRGPoolVariableDefinition from: sharedPools
]

{ #category : #'shared pools' }
RGClassDefinition >> removeSharedPoolNamed: poolName [

	self removeVariable: (self sharedPoolNamed: poolName) from: sharedPools
]

{ #category : #'managing pool users' }
RGClassDefinition >> removeUser: aRGClassDefinition [
	"Removes this RGClassDefinition from the users of the receiver"
	
	aRGClassDefinition isClass ifFalse:[ ^self ].
	aRGClassDefinition instanceSide removeSharedPoolNamed: self name.
	self users remove: aRGClassDefinition instanceSide ifAbsent:[ ]
]

{ #category : #'shared pools' }
RGClassDefinition >> sharedPoolNamed: poolName [

	^sharedPools detect:[ :v| v name = poolName asSymbol ] ifNone:[ nil ]
]

{ #category : #'shared pools' }
RGClassDefinition >> sharedPoolNames [
	
	^sharedPools collect:[ :pool| pool name ]
]

{ #category : #accessing }
RGClassDefinition >> sharedPools [
	"Keeps the pool variable relationship of the receiver"

	^ sharedPools
]

{ #category : #accessing }
RGClassDefinition >> sharedPools: aCollection [
	sharedPools := aCollection
]

{ #category : #accessing }
RGClassDefinition >> stamp [

	^self hasComment ifTrue:[ self comment stamp ] ifFalse:[ nil ]
]

{ #category : #accessing }
RGClassDefinition >> stamp: aString [

	self hasComment
	ifTrue: [ self comment stamp: aString ]
	ifFalse:[ RGCommentDefinition new
						 parent: self; 
						 stamp: aString ]
]

{ #category : #'accessing - deprecated parallel hierarchy ' }
RGClassDefinition >> theMetaClass [
	
	^metaClass
]

{ #category : #'accessing - deprecated parallel hierarchy ' }
RGClassDefinition >> theNonMetaClass [

	^self
]

{ #category : #'managing pool users' }
RGClassDefinition >> users [
	"If the reciever is a SharedPool then retrieves its users"
	
	^self isPool
		ifTrue: [ self annotationNamed: self class usersKey ifAbsentPut: [ OrderedCollection new ] ]
		ifFalse:[ #() ]
]

{ #category : #behavior }
RGClassDefinition >> withMetaclass [
	"Registers explicitly the metaclass of a class"

	metaClass:= RGMetaclassDefinition class: self

]

{ #category : #behavior }
RGClassDefinition >> withMetaclass: aRGMetaclassDefinition [
	"Registers explicitly the metaclass of a class"

	metaClass:= aRGMetaclassDefinition.
	metaClass baseClass: self.  

]
