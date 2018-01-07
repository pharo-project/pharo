"
repository := MCFileTreeRepository new
	directory: './mini2' asFileReference;
	yourself.
	
environment := repository asRing2Environment.		

environment removeAllButBehaviorsNamed: usedClasses.
environment removeAllButMethodsNamed: calledMethods.

environment removeEmptyMethodTags.
environment fixProtoObjectClassSuperclass.
environment addGlobalsNamed: #(#Smalltalk #SourceFiles #Transcript #Undeclared #Display #TextConstants  #Sensor #Processor #SystemOrganization).
environment clean.
	
environment bootstrap.




"
Class {
	#name : #RG2Environment,
	#superclass : #RG2Definition,
	#instVars : [
		'announcer',
		'behaviors',
		'packages',
		'globalVariables',
		'backend',
		'queryInterface'
	],
	#category : #'Ring2-Core-Environment'
}

{ #category : #'accessing - backend' }
RG2Environment >> addBehavior: aRG2BehaviorDefinition [

	self backend forEnvironment
		addBehavior: aRG2BehaviorDefinition
		to: self.
	aRG2BehaviorDefinition isMeta ifFalse: [
		aRG2BehaviorDefinition package
			ifNotNil: [ :package | 
				self backend forEnvironment addPackage: aRG2BehaviorDefinition package	to: self.
				package addDefinedBehavior: aRG2BehaviorDefinition. ].
		"self backend forEnvironment
					addGlobalVariable: (RG2GlobalVariableDefinition named: aRG2BehaviorDefinition name parent: self)
					to: self" ].

	(aRG2BehaviorDefinition hasResolved: #name)
		ifTrue: [ self queryInterface behaviorsDictionary at: aRG2BehaviorDefinition name asSymbol put: aRG2BehaviorDefinition ].
				

]

{ #category : #'accessing - backend' }
RG2Environment >> addGlobalVariable: aRG2GlobalVariableDefinition [ 

	self backend forBehavior addGlobalVariable: aRG2GlobalVariableDefinition to: self.
]

{ #category : #'accessing - backend' }
RG2Environment >> addPackage: aRG2Package [

	self backend forPackage addPackage: aRG2Package to: self.
	
	self announce: (RPackageRegistered to: aRG2Package).

]

{ #category : #accessing }
RG2Environment >> announcer [

	^ announcer ifNil: [ announcer := RG2EnvironmentAnnouncer new ]
]

{ #category : #accessing }
RG2Environment >> ask [ 

	^ queryInterface ifNil: [ queryInterface := RG2EnvironmentQueryInterface for: self ].
]

{ #category : #accessing }
RG2Environment >> backend [ 

	^ backend ifNil: [ backend := RG2EnvironmentBackend for: self ].
]

{ #category : #accessing }
RG2Environment >> backend: anRG2EnvironmentBackend [

	^ backend := anRG2EnvironmentBackend
]

{ #category : #'accessing - backend' }
RG2Environment >> behaviorsDo: aBlock [

	self backend forEnvironment behaviorsFor: self do: aBlock
]

{ #category : #binding }
RG2Environment >> bindingOf: aSymbol [
	
	| behavior result |
	
	self globalVariablesBindings at: aSymbol ifPresent: [ :val | ^ val ].
	
	self ask globalVariables detect: [ :each | each name = aSymbol  ] ifFound: [ :found | 
		self globalVariablesBindings at: aSymbol put: found.
		^ found ].

	behavior := self ask behaviorNamed: aSymbol.
	result := behavior ifNotNil: [ GlobalVariable key: aSymbol value: behavior].
	self globalVariablesBindings at: aSymbol put: result.
	^ result
]

{ #category : #'as yet unclassified' }
RG2Environment >> child: aChildDefinition renamedFrom: oldName to: newName [

	self queryInterface behaviorsDictionary removeKey: oldName ifAbsent: [].
	self queryInterface behaviorsDictionary at: newName asSymbol put: aChildDefinition.

]

{ #category : #cleaning }
RG2Environment >> clean [

	self cleanWithMetaclassNamed: #Metaclass

]

{ #category : #'accessing - backend' }
RG2Environment >> cleanBehaviors [

	| oldBehaviors |
	
	oldBehaviors := self ask behaviors.
	
	self backend forPackage cleanBehaviorsFor: self.
	
	oldBehaviors do: [ :each |
		self announcer behaviorRemoved: each ].
	
	self queryInterface resetBehaviorsDictionary.

]

{ #category : #'accessing - backend' }
RG2Environment >> cleanGlobalVariables [

	self backend forBehavior cleanGlobalVariablesFor: self.
]

{ #category : #'accessing - backend' }
RG2Environment >> cleanPackages [

	self backend forEnvironment cleanPackagesFor: self
]

{ #category : #cleaning }
RG2Environment >> cleanSuperclassesOfMetaclasses [

	"fix suprclasses of metaclasses do not pointing to metaclasses of superclasses"
	 
	self ask behaviorsDo: [ :each | 
		(each isMetaclass and: [ each superclass isRing2Resolved not ])
			ifTrue: [ each superclass: each baseClass superclass metaclass] ].

]

{ #category : #cleaning }
RG2Environment >> cleanUnusedUnreferencedBehaviors [

	"remove behaviors that are not linked in the graph of the named behaviors"
	
	| referenced unreferenced newelyAdded |
	
	referenced := IdentitySet new.
	self ask behaviorsDo: [ :each |
		(each hasResolved: #name) ifTrue: [ 
			referenced add: each.
			referenced addAll: (each referencedBehaviors copyWithoutAll: {each. nil}) ]  ].
	[	newelyAdded := IdentitySet new.
		(referenced copyWithout: nil) do: [ :each | 
			each referencedBehaviors do: [ :ref |
				(referenced includes: ref) ifFalse: [
					referenced add: ref.
					newelyAdded add: ref]]].
		newelyAdded isEmpty ] whileFalse.

	unreferenced := (self ask behaviors copyWithoutAll: referenced).

	unreferenced do: [:each | self removeBehavior: each  ].
	
	^ unreferenced
]

{ #category : #cleaning }
RG2Environment >> cleanUnusedUnreferencedPackages [

	"remove packages that are not not used"
	
	| referenced unreferenced |
	
	referenced := IdentitySet new.
	self ask behaviorsDo: [ :each |
		referenced addAll: each referencedPackages ].
	
	unreferenced := (self ask packages copyWithoutAll: referenced).

	unreferenced do: [:each | self removePackage: each  ].
	
	^ unreferenced
]

{ #category : #cleaning }
RG2Environment >> cleanWithMetaclassNamed: aProposedName [

	self unifyMetaclass: aProposedName.
	self hasTraits ifTrue: [ 
		self unifyTrait.
		self unifyClassTrait. ].
	self cleanSuperclassesOfMetaclasses.
	self cleanUnusedUnreferencedBehaviors.
	self cleanUnusedUnreferencedPackages.
]

{ #category : #'as yet unclassified' }
RG2Environment >> createDefaultEnvironment [

	| protoobject protoobjectClass object objectClass class classClass classDescription classDescriptionClass behavior behaviorClass metaclass metaclassClass kernelPackage |
	
	protoobject := RG2ClassDefinition unresolvedNamed: #'ProtoObject' withParent: self.
	protoobjectClass := RG2MetaclassDefinition unresolvedNamed: #'ProtoObject class' withParent: self.
	object := RG2ClassDefinition unresolvedNamed: #'Object' withParent: self.
	objectClass := RG2MetaclassDefinition unresolvedNamed: #'Object class' withParent: self.
	class := RG2ClassDefinition unresolvedNamed: #'Class' withParent: self.
	classClass := RG2MetaclassDefinition unresolvedNamed: #'Class class' withParent: self.
	classDescription := RG2ClassDefinition unresolvedNamed: #'ClassDescription' withParent: self.
	classDescriptionClass := RG2MetaclassDefinition unresolvedNamed: #'ClassDescription class' withParent: self.
	behavior := RG2ClassDefinition unresolvedNamed: #'Behavior' withParent: self.
	behaviorClass := RG2MetaclassDefinition unresolvedNamed: #'Behavior class' withParent: self.
	metaclass := RG2ClassDefinition unresolvedNamed: #'Metaclass' withParent: self.
	metaclassClass := RG2MetaclassDefinition unresolvedNamed: #'Metaclass class' withParent: self.
	kernelPackage := RG2PackageDefinition unresolvedNamed: #'Kernel' withParent: self.
	
	{ protoobject. protoobjectClass. object. objectClass. class. classClass. classDescription. classDescriptionClass. behavior. behaviorClass. metaclass. metaclassClass } do: [ :each |
		kernelPackage pvtAddDefinedBehavior: each.
		self pvtAddBehavior: each.].
	self pvtAddPackage: kernelPackage.
	
	protoobject pvtSuperclass: protoobject.
	protoobject pvtMetaclass: protoobjectClass.
	protoobject pvtPackage: kernelPackage.

	protoobjectClass pvtSuperclass: class.
	protoobjectClass pvtMetaclass: metaclass.

	object pvtSuperclass: protoobject.
	object pvtMetaclass: objectClass.
	object pvtPackage: kernelPackage.

	objectClass pvtSuperclass: protoobjectClass.
	objectClass pvtMetaclass: metaclass.

	class pvtSuperclass: classDescription.
	class pvtMetaclass: classClass.
	class pvtPackage: kernelPackage.

	classClass pvtSuperclass: classDescriptionClass.
	classClass pvtMetaclass: metaclass.

	classDescription pvtSuperclass: behavior.
	classDescription pvtMetaclass: classDescriptionClass.
	classDescription pvtPackage: kernelPackage.

	classDescriptionClass pvtSuperclass: behaviorClass.
	classDescriptionClass pvtMetaclass: metaclass.

	behavior pvtSuperclass: object.
	behavior pvtMetaclass: behaviorClass.
	behavior pvtPackage: kernelPackage.

	behaviorClass pvtSuperclass: objectClass.
	behaviorClass pvtMetaclass: metaclass.

	metaclass pvtSuperclass: classDescription.
	metaclass pvtMetaclass: metaclassClass.
	metaclass pvtPackage: kernelPackage.

	metaclassClass pvtSuperclass: classDescriptionClass.
	metaclassClass pvtMetaclass: metaclass.


		
]

{ #category : #'default model values' }
RG2Environment >> defaultBehaviors [

	^ IdentitySet new
]

{ #category : #'default model values' }
RG2Environment >> defaultGlobalVariables [

	^ IdentitySet new
]

{ #category : #'default model values' }
RG2Environment >> defaultPackages [

	^ IdentitySet new
]

{ #category : #accessing }
RG2Environment >> definitionFor: anObject [

	^ self backend definitionFor: anObject.
]

{ #category : #'as yet unclassified' }
RG2Environment >> ensureClassNamed: aSymbol [

	self assert: (aSymbol endsWith: ' classTrait') not.

	^ self ask behaviorNamedExactlyAs: aSymbol ifAbsent: [ 
		 | behaviorClass newBehavior sibling siblingName |
		siblingName := (aSymbol endsWith: ' class') 
			ifTrue: [ aSymbol withoutSuffix: ' class' ]
			ifFalse: [ (aSymbol, 'class') asSymbol ].
				
			sibling := self ask behaviors detect: [ :each | each name = siblingName ] ifNone: nil.
			sibling ifNotNil: [ 
				| result |
				result := sibling isMetaclass 
					ifTrue: [ sibling baseClass ]
					ifFalse: [ sibling metaclass ].
				result name: aSymbol.
				result propertyNamed: #resolved put: true.
				^ result ].
			
			behaviorClass := (aSymbol endsWith: ' class') 
				ifFalse: [ RG2ClassDefinition ]
				ifTrue: [ RG2MetaclassDefinition ].
			newBehavior := behaviorClass named: aSymbol parent: self.
			self addBehavior: newBehavior.
			newBehavior ] 
]

{ #category : #cleaning }
RG2Environment >> ensureClassTrait [

	| aClassTrait |
	
	aClassTrait := self ask behaviorNamed: #ClassTrait.
	aClassTrait ifNotNil: [ ^ aClassTrait ].
	
	aClassTrait := self ask behaviors detect: [ :each | 
		(each propertyNamed: #ClassTrait ifAbsent: [ false ]) ] ifNone: [ nil ].
	aClassTrait ifNotNil: [ ^ aClassTrait ].

	aClassTrait := self ensureClassNamed: #ClassTrait.	
	
	aClassTrait propertyNamed: #ClassTrait put: true.	
	
	^ aClassTrait.
]

{ #category : #cleaning }
RG2Environment >> ensureMetaclass [

	| aMetaclass |
	
	aMetaclass := self ask behaviorNamed: #Metaclass.
	aMetaclass ifNotNil: [ ^ aMetaclass ].
	
	aMetaclass := self ask behaviors detect: [ :each | 
		(each propertyNamed: #Metaclass ifAbsent: [ false ]) ] ifNone: [ nil ].
	aMetaclass ifNotNil: [ ^ aMetaclass ].

	aMetaclass := self ensureClassNamed: #Metaclass.	
	
	aMetaclass propertyNamed: #Metaclass put: true.	
	
	^ aMetaclass.
]

{ #category : #cleaning }
RG2Environment >> ensureMetaclass: aProposedName [

	| aMetaclass |
	
	aMetaclass := self ask behaviorNamed: aProposedName.
	aMetaclass ifNotNil: [ ^ aMetaclass ].
	
	aMetaclass := self ask behaviors detect: [ :each | 
		(each propertyNamed: #Metaclass ifAbsent: [ false ]) ] ifNone: [ nil ].
	aMetaclass ifNotNil: [ ^ aMetaclass ].

	aMetaclass := self ensureClassNamed: #Metaclass.	
	
	aMetaclass propertyNamed: #Metaclass put: true.	
	
	^ aMetaclass.
]

{ #category : #'as yet unclassified' }
RG2Environment >> ensureMetaclassTraitNamed: aSymbol [

	| found newTrait newMetaclassTrait similarMetaclass similarMetaclassName siblingName sibling originalMetaclass |
	
	self assert: (aSymbol endsWith: ' class') not.
	self assert: (aSymbol endsWith: ' classTrait').

	found := self ask behaviorNamedExactlyAs: aSymbol.
	(found notNil and: [ found isMetaclassTrait ]) ifTrue: [ ^ found ].		

	^ found 
		ifNil: [ 
			"trait not found, check if the environment contains already a correspoinding trait"
			siblingName := (aSymbol withoutSuffix: ' classTrait') asSymbol.
			sibling := self ask behaviorNamedExactlyAs: siblingName.
			sibling 
				ifNotNil: [ 
					"resolve and return existing corresponding trait"
					| existing |
					sibling isTrait
						ifTrue: [ 
							existing := sibling classTrait.
							existing name: aSymbol.
							existing propertyNamed: #resolved put: true.
							existing ]
						ifFalse: [ 
							newTrait := sibling pvtAsTrait.
							originalMetaclass := sibling metaclass.
							newMetaclassTrait := originalMetaclass pvtAsMetaclassTrait.
							self ask replaceName: originalMetaclass name with: newMetaclassTrait name.
							sibling metaclass becomeForward: newMetaclassTrait.
							originalMetaclass becomeForward: newMetaclassTrait.
							sibling becomeForward: newTrait.
							sibling classTrait: newMetaclassTrait.
							newMetaclassTrait baseTrait: sibling.
							newMetaclassTrait name: (aSymbol, ' classTrait') asSymbol.
							newMetaclassTrait propertyNamed: #resolved put: true.
							newMetaclassTrait
							]]
				ifNil: [ 
					similarMetaclassName := ((aSymbol withoutSuffix: ' classTrait'), ' class') asSymbol.
					similarMetaclass := self ask behaviorNamed: similarMetaclassName.	
					similarMetaclass 
						ifNotNil: [
							"environment already contains metaclass of the similar name. It was probably 
							created because correct type was not known. Convert it to classTrait"
							| similarClass |
							newMetaclassTrait := similarMetaclass pvtAsMetaclassTrait.
							similarClass := similarMetaclass baseClass.
							newTrait := similarClass pvtAsTrait.
							self ask replaceName: similarMetaclass name with: newMetaclassTrait name.
							similarMetaclass becomeForward: newMetaclassTrait.
							similarClass becomeForward: newTrait.
							similarClass classTrait: similarMetaclass.
							similarClass name: (aSymbol withoutSuffix: ' classTrait') asSymbol.
							similarMetaclass baseTrait: similarClass.
							similarMetaclass ]
						ifNil: [ 
							"we need to create new behavior"
							| newBehavior |
							newBehavior := RG2MetaclassTraitDefinition named: aSymbol parent: self.
							self addBehavior: newBehavior.
							newBehavior ] ] ]
		ifNotNil: [
			"some non-trait behavior found. We created this trait before as a class because correct behavior type was not known."
			newTrait := found pvtAsTrait.
			originalMetaclass := found metaclass.
			newMetaclassTrait := originalMetaclass pvtAsMetaclassTrait.
			self ask replaceName: originalMetaclass name with: newMetaclassTrait name.
			found becomeForward: newTrait.
			found metaclass becomeForward: newMetaclassTrait.
			originalMetaclass becomeForward: newMetaclassTrait.
			found ].
	
				

]

{ #category : #'as yet unclassified' }
RG2Environment >> ensurePackageNamed: aSymbol [

	^ self ask packages 
		detect: [:each | each name = aSymbol] 
		ifNone: [ | newPackage |
				newPackage := RG2PackageDefinition named: aSymbol parent: self.
				self addPackage: newPackage.
				newPackage ].
			

]

{ #category : #cleaning }
RG2Environment >> ensureTrait [

	| aTrait |
	
	aTrait := self ask behaviorNamed: #Trait.
	aTrait ifNotNil: [ ^ aTrait ].
	
	aTrait := self ask behaviors detect: [ :each | 
		(each propertyNamed: #Trait ifAbsent: [ false ]) ] ifNone: [ nil ].
	aTrait ifNotNil: [ ^ aTrait ].

	aTrait := self ensureClassNamed: #Trait.	
	
	aTrait propertyNamed: #Trait put: true.	
	
	^ aTrait.
]

{ #category : #'as yet unclassified' }
RG2Environment >> ensureTraitNamed: aSymbol [

	| found newTrait newMetaclassTrait siblingName sibling originalMetaclass |
	
	self assert: (aSymbol endsWith: ' class') not.

	"process classTraits"
	(aSymbol endsWith: ' classTrait') ifTrue: [ 
		^ self ensureMetaclassTraitNamed: aSymbol ].
	
	"a trait with the same name already exists"
	found := self ask behaviorNamedExactlyAs: aSymbol.
	(found notNil and: [ found isTrait ]) ifTrue: [ ^ found ].
	
	^ found 
		ifNil: [ 
			"trait not found, check if the environment contains already a correspoinding metaclass trait"
			siblingName := (aSymbol, ' classTrait') asSymbol.
			sibling := self ask behaviorNamedExactlyAs: siblingName.
			sibling 
				ifNotNil: [ 
					"resolve and return existing corresponding trait"
					| existing |
					existing := sibling baseTrait.
					existing name: aSymbol.
					existing propertyNamed: #resolved put: true.
					existing ]
				ifNil: [ 
					"we need to create new behavior"
					| newBehavior |
					newBehavior := RG2TraitDefinition named: aSymbol parent: self.
					self addBehavior: newBehavior.
					newBehavior ] ]
		ifNotNil: [
			"some non-trait behavior found. We created this trait before as a class because correct behavior type was not known."
			newTrait := found pvtAsTrait.
			originalMetaclass := found metaclass.
			newMetaclassTrait := originalMetaclass pvtAsMetaclassTrait.
			self ask replaceName: originalMetaclass name with: newMetaclassTrait name.
			found becomeForward: newTrait.
			found classTrait becomeForward: newMetaclassTrait.
			originalMetaclass becomeForward: newMetaclassTrait.
			found ].
	
	
]

{ #category : #'unpackaged ' }
RG2Environment >> ensureUnpackagedPackage [

	^ self unpackagedPackageOrNil
		ifNotNil: [ :found | found ]
		ifNil: [ | newPackage |
			newPackage := RG2PackageDefinition unresolvedWithParent: self.
			newPackage pvtName: self unpackagedPackageName.
			self pvtAddPackage: newPackage.
			^ newPackage ]
]

{ #category : #accessing }
RG2Environment >> environment [ 

	^ self
]

{ #category : #'as yet unclassified' }
RG2Environment >> fixProtoObjectClassSuperclass [

	(self ask classNamed: #'ProtoObject class') superclass: (self ask classNamed: #Class).

]

{ #category : #'as yet unclassified' }
RG2Environment >> globalVariablesBindings [

	^ self propertyNamed: #globalVariablesBindings ifAbsentPut: [ IdentityDictionary new.]
]

{ #category : #'accessing - backend' }
RG2Environment >> globalVariablesDo: aBlock [

	self backend forBehavior globalVariablesFor: self do: aBlock
]

{ #category : #'as yet unclassified' }
RG2Environment >> hasTraits [

	self behaviorsDo: [ :each |
		each isTrait ifTrue: [ ^ true ] ].
	
	^ false
]

{ #category : #initialization }
RG2Environment >> initialize [

	super initialize.

	behaviors := self unresolvedValue: self defaultBehaviors.
	packages := self unresolvedValue: self defaultPackages.
	globalVariables := self unresolvedValue: self defaultGlobalVariables.
	

]

{ #category : #initialization }
RG2Environment >> initializeUnresolved [

	super initializeUnresolved.

	behaviors := self unresolvedValue: self defaultBehaviors.
	packages := self unresolvedValue: self defaultPackages.
	globalVariables := self unresolvedValue: self defaultGlobalVariables.
	

]

{ #category : #'testing types' }
RG2Environment >> isEnvironment [

	^ true
]

{ #category : #merging }
RG2Environment >> merge: aRG2Package [

	super merge: aRG2Package.
	
	((self hasUnresolved: #definedBehaviors) and: [ (aRG2Package hasUnresolved: #definedBehaviors) ])
		ifFalse: [ 
			(self hasResolved: #definedBehaviors) ifFalse: [ 
				self cleanDefinedBehaviors ].
			aRG2Package definedBehaviorsDo: [ :behavior |
				| newDefinition |
				 "create empty unresolved definition of the same kind in the same environment"
				newDefinition := behavior class unresolvedWithParent: self.
				newDefinition merge: behavior.
				self addDefinedBehavior: newDefinition ]].
	
	"TODO:extension methods"
]

{ #category : #accessing }
RG2Environment >> package [

	^ nil
]

{ #category : #'accessing - backend' }
RG2Environment >> packagesDo: aBlock [

	self backend  packagesFor: self do: aBlock
]

{ #category : #'private - backend interface' }
RG2Environment >> pvtAddBehavior: aRG2BehaviorDefinition [

	self environment verifyOwnership: aRG2BehaviorDefinition.
	
	behaviors isRing2Resolved ifFalse: [ 
		self pvtCleanBehaviors ].
	
	(behaviors includes: aRG2BehaviorDefinition)
		ifFalse: [ 
			behaviors add: aRG2BehaviorDefinition.
			self announcer behaviorAdded: aRG2BehaviorDefinition].
		
	
		
]

{ #category : #'private - backend access' }
RG2Environment >> pvtAddGlobalVariable: aRG2GlobalVariableDefinition [

	self environment verifyOwnership: aRG2GlobalVariableDefinition.
	
	globalVariables isRing2Resolved ifFalse: [ 
		self pvtCleanGlobalVariables  ].
	
	globalVariables add: aRG2GlobalVariableDefinition.	
]

{ #category : #'private - backend interface' }
RG2Environment >> pvtAddPackage: aRG2Package [

	self environment verifyOwnership: aRG2Package.
	
	packages isRing2Resolved ifFalse: [ 
		self pvtCleanPackages ].
	
	packages add: aRG2Package.	
		
]

{ #category : #'private - backend interface' }
RG2Environment >> pvtBehaviorsDo: aBlock [

	^ behaviors value do: aBlock
]

{ #category : #'private - backend interface' }
RG2Environment >> pvtCleanBehaviors [

	behaviors := self defaultBehaviors.
		
	"TODO:Announce if not empty"
	
	
]

{ #category : #'private - backend access' }
RG2Environment >> pvtCleanGlobalVariables [

	globalVariables := self defaultGlobalVariables.
]

{ #category : #'private - backend interface' }
RG2Environment >> pvtCleanPackages [

	packages := self defaultPackages.
		
	"TODO:Announce if not empty"
	
	
]

{ #category : #'private - backend access' }
RG2Environment >> pvtGlobalVariablesDo: aBlock [

	globalVariables value do: aBlock
]

{ #category : #'private - backend interface' }
RG2Environment >> pvtPackagesDo: aBlock [

	^ packages value do: aBlock
]

{ #category : #'private - backend interface' }
RG2Environment >> pvtRemoveBehavior: aRG2BehaviorDefinition [

	self verifyOwnership: aRG2BehaviorDefinition.
	
	behaviors remove: aRG2BehaviorDefinition.
	
	"TODO:Announce"
	
	
]

{ #category : #'private - backend access' }
RG2Environment >> pvtRemoveGlobalVariable: aRG2GlobalVariableDefinition [

	self environment verifyOwnership: aRG2GlobalVariableDefinition.
	
	globalVariables remove: aRG2GlobalVariableDefinition.
]

{ #category : #'private - backend interface' }
RG2Environment >> pvtRemovePackage: aRG2Package [

	self verifyOwnership: aRG2Package.
	
	packages remove: aRG2Package.
	
	"TODO:Announce"
	
	
]

{ #category : #'private - backend interface' }
RG2Environment >> pvtResolvableProperties [

	^ super pvtResolvableProperties, { 
		#behaviors -> behaviors. 
		#packages -> packages. 
		#globalVariables -> globalVariables. 
	}
	
	
]

{ #category : #accessing }
RG2Environment >> queryInterface [

	^ self ask
]

{ #category : #'accessing - backend' }
RG2Environment >> removeBehavior: aRG2BehaviorDefinition [

	self backend forEnvironment removeBehavior: aRG2BehaviorDefinition from: self.
	
	"remove extensions methods deleted with the behavior from owning packages"
	aRG2BehaviorDefinition extensions do: [ :each |
		each package removeExtensionMethod: each ].
	
	(aRG2BehaviorDefinition package notNil and: [aRG2BehaviorDefinition package  definedBehaviors includes: aRG2BehaviorDefinition]) ifTrue: [ 
	aRG2BehaviorDefinition package removeDefinedBehavior: aRG2BehaviorDefinition.
	].
	
	self announcer behaviorRemoved: aRG2BehaviorDefinition.
	
	(aRG2BehaviorDefinition hasResolved: #name) ifTrue: [ 
		self queryInterface behaviorsDictionary removeKey: aRG2BehaviorDefinition name ifAbsent: []].

]

{ #category : #'accessing - backend' }
RG2Environment >> removeGlobalVariable: aRG2GlobalVariableDefinition [

	self backend forBehavior removeGlobalVariable: aRG2GlobalVariableDefinition from: self
]

{ #category : #'accessing - backend' }
RG2Environment >> removePackage: aRG2Package [

	self backend forPackage removePackage: aRG2Package from: self.
	
	self announce: (RPackageUnregistered to: aRG2Package).

]

{ #category : #'unpackaged ' }
RG2Environment >> removeUnusedPackages [

	"remove all packages that are not used in the system. It cannot be done automatically
	for every change in the package structure because then it would be impossible to 
	create e.g. an environment with one empty package"
	
	| usedPackages | 
	
	usedPackages := IdentitySet new.
	self behaviorsDo: [ :behavior |
		behavior isMeta 
			ifFalse: [ usedPackages add: behavior package]. 
		behavior localMethodsDo: [ :method |
			  usedPackages add: method package]].
		
	self ask packages copy do: [ :each |
		(usedPackages includes: each) 
			ifFalse: [ self removePackage: each ] ].
]

{ #category : #cleaning }
RG2Environment >> unifyClassTrait [

	"set all metaclasses of classTraits to the same object (ClassTrait) "

	| aTrait |

	aTrait := self ensureClassTrait.
	 
	self ask behaviorsDo: [ :each | 
		(each isMetaclassTrait)
			ifTrue: [ each metaclass: aTrait]].

]

{ #category : #cleaning }
RG2Environment >> unifyMetaclass [

	"set all metaclasses of metaclasses to the same object (Metaclass) "

	| aMetaclass |

	aMetaclass := self ensureMetaclass.
	 
	self ask behaviorsDo: [ :each | 
		(each isMetaclass)
			ifTrue: [ each metaclass: aMetaclass] ].

]

{ #category : #cleaning }
RG2Environment >> unifyMetaclass: aProposedName [

	"set all metaclasses of metaclasses to the same object (Metaclass) "

	| aMetaclass |

	aMetaclass := self ensureMetaclass: aProposedName.
	 
	self ask behaviorsDo: [ :each | 
		(each isMetaclass)
			ifTrue: [ each metaclass: aMetaclass] ].

]

{ #category : #cleaning }
RG2Environment >> unifyTrait [

	"set all metaclasses of metaclasses to the same object (Metaclass) "

	| aTrait |

	aTrait := self ensureTrait.
	 
	self ask behaviorsDo: [ :each | 
		(each isTrait)
			ifTrue: [ each metaclass: aTrait]].
		
	(self ask behaviors select: [ :each | each isRing2Resolved not and: [(each propertyNamed: #role) = #trait] ]) do: [:each | self removeBehavior: each  ].
			

]

{ #category : #'unpackaged ' }
RG2Environment >> unpackagedPackage [

	| aProtocol |
	aProtocol := RG2PackageDefinition unresolvedWithParent: self.
	aProtocol pvtName: self unpackagedPackageName.
	
	^ aProtocol

]

{ #category : #'unpackaged ' }
RG2Environment >> unpackagedPackageName [

	^ '_UnpackagedPackage' asSymbol
]

{ #category : #'unpackaged ' }
RG2Environment >> unpackagedPackageOrNil [

	self packagesDo: [ :each |
		(each name = self unpackagedPackageName) ifTrue: [ ^ each ] ].
	
	^ nil.

]

{ #category : #utility }
RG2Environment >> verifyOwnership: aRG2Definition [

	"ignore unresolved values. TODO: check default values ownership?"
	aRG2Definition isRing2Resolved ifFalse: [ ^ self.].

	self assert: (aRG2Definition environment = self).
]
