"
I know how to create classes in the system.
The classes are nor installed or modifies other objects. That is part of the job of the ShiftClassInstaller.

I can be extended by using a different builder enhancer. 
See ShDefaultBuilderEnhancer for a default implementation. 

I can be used directly to create anonymous classes, but it is better if you use the annonymous class installer.

I also can compare the old class with the configured new class to calculate the required changes.
"
Class {
	#name : #ShiftClassBuilder,
	#superclass : #Object,
	#instVars : [
		'buildEnvironment',
		'superclassName',
		'name',
		'layoutDefinition',
		'comment',
		'commentStamp',
		'superclass',
		'category',
		'newMetaclass',
		'newClass',
		'oldClass',
		'oldMetaclass',
		'builderEnhancer',
		'metaclassClass',
		'extensibleProperties',
		'changeComparers',
		'changes',
		'metaSuperclass'
	],
	#classVars : [
		'BuilderEnhancer'
	],
	#category : #'Shift-ClassBuilder'
}

{ #category : #'as yet unclassified' }
ShiftClassBuilder class >> defaultBuildEnhancer [
	^ BuilderEnhancer ifNil: [ ShDefaultBuilderEnhancer ]
]

{ #category : #accessing }
ShiftClassBuilder class >> setDefaultBuilderEnhancer: aClass [ 
	BuilderEnhancer := aClass
]

{ #category : #changes }
ShiftClassBuilder >> addChange: aChange [
	changes add: aChange.
]

{ #category : #changes }
ShiftClassBuilder >> addChangeComparer: aChangeComparer [
	changeComparers add: aChangeComparer.
]

{ #category : #building }
ShiftClassBuilder >> build [	
	self tryToFillOldClass.
	self detectBuilderEnhancer.
	
	self validateSuperclass.
	self compareWithOldClass.

	self layoutDefinition copySlotsIfUsedIn: oldClass.

	self createMetaclass.
	self createClass.

	self createSharedVariables.

	self oldClass ifNotNil: [ 
			self newClass basicCategory: self oldClass basicCategory.
			self copyOrganization.
			self builderEnhancer compileMethodsFor: self. ].

	self builderEnhancer afterMethodsCompiled: self.
	
	self installSlots.
	
	^ newClass
]

{ #category : #accessing }
ShiftClassBuilder >> buildEnvironment [
	^ buildEnvironment
]

{ #category : #accessing }
ShiftClassBuilder >> buildEnvironment: anObject [
	buildEnvironment := anObject
]

{ #category : #accessing }
ShiftClassBuilder >> builderEnhancer [
	^ builderEnhancer ifNil: [ self detectBuilderEnhancer ]
]

{ #category : #accessing }
ShiftClassBuilder >> builderEnhancer: anObject [
	builderEnhancer := anObject.
	
	builderEnhancer initializeBuilder: self
]

{ #category : #accessing }
ShiftClassBuilder >> category [
	^ category
]

{ #category : #accessing }
ShiftClassBuilder >> category: anObject [
	category := anObject
]

{ #category : #accessing }
ShiftClassBuilder >> changeComparers [
	^ changeComparers
]

{ #category : #accessing }
ShiftClassBuilder >> changes [
	^ changes
]

{ #category : #accessing }
ShiftClassBuilder >> classNamed: aName [
	^ self buildEnvironment classNamed:aName.
]

{ #category : #accessing }
ShiftClassBuilder >> classSlots: aSlotCollection [
	self layoutDefinition classSlots: aSlotCollection.
]

{ #category : #accessing }
ShiftClassBuilder >> classTraitComposition [
	^ extensibleProperties at: #classTraitComposition ifAbsent: {}
]

{ #category : #accessing }
ShiftClassBuilder >> classTraitComposition: aValue [
	^ extensibleProperties at: #classTraitComposition put: aValue.
]

{ #category : #accessing }
ShiftClassBuilder >> comment [
	^ comment
]

{ #category : #accessing }
ShiftClassBuilder >> comment: anObject [
	comment := anObject
]

{ #category : #'accessing comment' }
ShiftClassBuilder >> comment:aComment stamp: anStamp [
	self comment: aComment.
	self commentStamp: anStamp.
]

{ #category : #accessing }
ShiftClassBuilder >> commentStamp [
	^ commentStamp
]

{ #category : #accessing }
ShiftClassBuilder >> commentStamp: anObject [
	commentStamp := anObject
]

{ #category : #changes }
ShiftClassBuilder >> compareWithOldClass [
	oldClass ifNil: [ ^ self ].
	changeComparers do: [ :e | e compareClass: oldClass with: self ].
	
	changes ifEmpty: [ ShNoChangesInClass signal. ].
]

{ #category : #compiling }
ShiftClassBuilder >> compileMethods [
	newClass compileAllFrom: self oldClass.

]

{ #category : #copying }
ShiftClassBuilder >> copyClassSlotsFromExistingClass [
	"Ugly helper method: I copy over the class slots of the class in my builder environment."

	| anOldClass oldSlots |
	
	anOldClass := self environment at: name ifAbsent: [ ^ self ].
	oldSlots := anOldClass class classLayout slotScope visibleSlots.
	
	self layoutDefinition copyClassSlotsIfNeeded: oldSlots

]

{ #category : #building }
ShiftClassBuilder >> copyOrganization [
	newClass organization copyFrom: oldClass organization.
	newClass class organization copyFrom: oldClass class organization.
]

{ #category : #installing }
ShiftClassBuilder >> createClass [
	newClass := newMetaclass new.
	newClass setName: self name.

	self builderEnhancer 
		configureClass: newClass
		superclass: self superclass
		withLayoutType: self layoutDefinition layoutClass
		slots: self layoutDefinition slots.

	newClass environment: self buildEnvironment environment.
	
	self builderEnhancer classCreated: self.
]

{ #category : #building }
ShiftClassBuilder >> createMetaclass [
	newMetaclass := self metaclassClass new.
	
	self builderEnhancer 
		configureMetaclass: newMetaclass 	
		superclass: self metaSuperclass
		withLayoutType: FixedLayout
		slots: self layoutDefinition classSlots.
		
	self builderEnhancer metaclassCreated: self.
]

{ #category : #building }
ShiftClassBuilder >> createSharedVariables [
	self builderEnhancer 
		on: newClass
		declareClassVariables: self layoutDefinition sharedVariables
		sharing: self layoutDefinition sharedPoolsString.
	
]

{ #category : #initialization }
ShiftClassBuilder >> detectBuilderEnhancer [
	
	| builderEnhancerClass |
	
	builderEnhancerClass := ShDefaultBuilderEnhancer allSubclasses detect: [ :e | e isApplicableFor: self ] ifNone: [ self class defaultBuildEnhancer ].
	builderEnhancer := builderEnhancerClass new.
	
	builderEnhancer initializeBuilder: self.
	^ builderEnhancer
]

{ #category : #'reflective operations' }
ShiftClassBuilder >> doesNotUnderstand: aMessage [
	| selector variable setter|
		
	selector := aMessage selector.

	(selector isUnary or:[ selector isKeyword and:[ selector keywords size = 1] ]) 
		ifFalse:[ ^ super doesNotUnderstand: aMessage].

	setter := selector isKeyword.
	variable := setter ifTrue:[(selector allButLast: 1) asSymbol] ifFalse:[selector]. 
	
	(extensibleProperties includesKey: variable) 
		ifFalse: [ ^ super doesNotUnderstand: aMessage ].
	
	setter 
		ifTrue:[ extensibleProperties at: variable put: aMessage argument] 
		ifFalse:[ ^ extensibleProperties at: variable].
	
]

{ #category : #accessing }
ShiftClassBuilder >> environment [
	^ buildEnvironment environment
]

{ #category : #accessing }
ShiftClassBuilder >> environment: anEnvironment [
	^ buildEnvironment environment: anEnvironment
]

{ #category : #initialization }
ShiftClassBuilder >> fillFor: aClass [
	self
		superclass: aClass superclass;
		name: aClass getName;
		layoutClass: aClass classLayout class;
		slots: aClass localSlots ;
		sharedVariablesFromString: aClass classVariablesString;
		sharedPools: aClass sharedPoolsString;
		category: aClass category;
		environment: aClass environment;
		copyClassSlotsFromExistingClass;
		oldClass: aClass.
		
	self builderEnhancer fillBuilder: self from: aClass.

]

{ #category : #testing }
ShiftClassBuilder >> hasToMigrateInstances [
	^ self changes anySatisfy: [ :e | e hasToMigrateInstances ]
]

{ #category : #initialization }
ShiftClassBuilder >> initialize [
	super initialize.
	
	superclassName := #Object.

	layoutDefinition := ShLayoutDefinition new.
	layoutDefinition builder: self.

	extensibleProperties := Dictionary new.
	
	changeComparers := OrderedCollection new.
	changes := Set new.
]

{ #category : #building }
ShiftClassBuilder >> installSlots [
	newClass classLayout slots do: [ :each | each installingIn: newClass ].
	newClass classLayout slots do: [ :each | each layoutChanged: newClass ].
	
	newClass class classLayout slots do: [ :each | each installingIn: newClass class ].
	newClass class classLayout slots do: [ :each | each layoutChanged: newClass class ].
]

{ #category : #accessing }
ShiftClassBuilder >> layoutClass: aLayoutClass [
	self layoutDefinition layoutClass: aLayoutClass.
]

{ #category : #accessing }
ShiftClassBuilder >> layoutDefinition [
	^ layoutDefinition
]

{ #category : #accessing }
ShiftClassBuilder >> metaSuperclass [

	^ metaSuperclass ifNil:[ (superclass ifNil: [ Class ] ifNotNil: [ superclass class ]) ]
]

{ #category : #accessing }
ShiftClassBuilder >> metaSuperclass: aClass [
	
	metaSuperclass := aClass
]

{ #category : #accessing }
ShiftClassBuilder >> metaclassClass [
	^ metaclassClass ifNil: [ Metaclass ]
]

{ #category : #accessing }
ShiftClassBuilder >> metaclassClass: anObject [
	metaclassClass := anObject
]

{ #category : #accessing }
ShiftClassBuilder >> name [
	^ name
]

{ #category : #accessing }
ShiftClassBuilder >> name: anObject [
	name := anObject.
	self validateClassName.
]

{ #category : #accessing }
ShiftClassBuilder >> newClass [
	^ newClass
]

{ #category : #accessing }
ShiftClassBuilder >> newMetaclass [
	^ newMetaclass
]

{ #category : #changes }
ShiftClassBuilder >> notifyChanges [
	changes do: #announceChanges
]

{ #category : #accessing }
ShiftClassBuilder >> oldClass [
	^ oldClass
]

{ #category : #accessing }
ShiftClassBuilder >> oldClass: anObject [
	oldClass := anObject.
	oldClass ifNotNil: [oldMetaclass := oldClass class].
]

{ #category : #accessing }
ShiftClassBuilder >> oldMetaclass [
	^ oldMetaclass
]

{ #category : #changes }
ShiftClassBuilder >> propagateChangesTo: anotherBuilder [
	changes do: [ :e | e propagateToSubclasses: anotherBuilder ]
]

{ #category : #accessing }
ShiftClassBuilder >> propertyAt: aKey [ 
	^ extensibleProperties at: aKey.
]

{ #category : #accessing }
ShiftClassBuilder >> propertyAt: aKey put: aValue [
	extensibleProperties at: aKey put: aValue.
]

{ #category : #accessing }
ShiftClassBuilder >> sharedPools: aString [
	self layoutDefinition sharedPools: ((aString substrings: ' ') collect: [:e | e asSymbol])
]

{ #category : #accessing }
ShiftClassBuilder >> sharedVariables: aCollection [
	self layoutDefinition sharedVariables: (aCollection collect:[:e | e asClassVariable]).
]

{ #category : #accessing }
ShiftClassBuilder >> sharedVariablesFromString: aStringOrArray [
	layoutDefinition sharedVariables: (aStringOrArray isString
        ifTrue: [ (aStringOrArray substrings: ' ') collect: [ :x | x asSymbol => ClassVariable ] ]
        ifFalse: [ aStringOrArray collect: [ :x | 
                x isSymbol
                    ifTrue: [ x => ClassVariable ]
                    ifFalse: [ self error: 'Shared variables can only be String or an array of Symbols' ] ] ])
]

{ #category : #accessing }
ShiftClassBuilder >> slots: aCollection [
	self layoutDefinition slots: aCollection.
]

{ #category : #accessing }
ShiftClassBuilder >> superclass [
	^ superclass ifNil:[ superclassName ifNotNil: [buildEnvironment classNamed: (self superclassName)]]
]

{ #category : #accessing }
ShiftClassBuilder >> superclass: aSuperclass [
	aSuperclass ifNil:[superclassName := nil].
	superclass := aSuperclass
]

{ #category : #accessing }
ShiftClassBuilder >> superclassName [
	^ superclassName
]

{ #category : #accessing }
ShiftClassBuilder >> superclassName: anObject [
	superclassName := anObject
]

{ #category : #accessing }
ShiftClassBuilder >> traitComposition [
	^ extensibleProperties at: #traitComposition ifAbsent: {}
]

{ #category : #accessing }
ShiftClassBuilder >> traitComposition: aValue [
	^ extensibleProperties at: #traitComposition put: aValue.
]

{ #category : #building }
ShiftClassBuilder >> tryToFillOldClass [
	oldClass ifNotNil: [ ^self ].
	name ifNil: [ ^self ].
	self oldClass: (self buildEnvironment classNamed: self name)
]

{ #category : #'private validating' }
ShiftClassBuilder >> validateClassName [
	name ifNil: [ ^self ].
	
	"I try to convert to symbol, if there is an error the next guard will catch it"
	[name := name asSymbol ] on:Error do: [  ].
	
	name isSymbol ifFalse:[InvalidGlobalName 
				signal: 'Global names should be symbols' 
				for: name].

	name isValidGlobalName ifFalse: [ 
		InvalidGlobalName signal: 'Class name is not a valid global name. It must start with uppercase letter and continue with alphanumeric characters or underscore. Default names used in class or trait templates are not allowed.' for: name  ].
	
		
	DangerousClassNotifier check: name 
]

{ #category : #'private validating' }
ShiftClassBuilder >> validateSuperclass [
	self superclass ifNil: [ ^self ].
	oldClass ifNil: [ ^ self ].	
	
	self superclass withAllSuperclassesDo: [ :aSuperclass |
		aSuperclass = oldClass ifTrue:[
			CircularHierarchyError signalFor: oldClass. 
		]
	].	

]
