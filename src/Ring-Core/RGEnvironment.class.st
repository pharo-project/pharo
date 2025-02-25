"
I'm a representation of a Pharo environment that contains behaviors, packages and global variables. I have my own announcer for processing of events inside of the environment. 

I'm connected to a data source - backend, that provides me real data I'm working with. 

In most cases, you should communicate with me using my query interface. You will get it by sending me a message #ask.

"
Class {
	#name : 'RGEnvironment',
	#superclass : 'RGObject',
	#instVars : [
		'announcer',
		'behaviors',
		'packages',
		'globalVariables',
		'backend',
		'queryInterface',
		'version'
	],
	#category : 'Ring-Core-Environment',
	#package : 'Ring-Core',
	#tag : 'Environment'
}

{ #category : 'accessing - backend' }
RGEnvironment >> addBehavior: anRGBehavior [

	self backend forEnvironment
		addBehavior: anRGBehavior
		to: self.
	anRGBehavior isMeta ifFalse: [
		anRGBehavior package
			ifNotNil: [ :package |
				self backend forEnvironment addPackage: anRGBehavior package	to: self.
				package addDefinedBehavior: anRGBehavior. ].
		"self backend forEnvironment
					addGlobalVariable: (RGGlobalVariable named: anRGBehavior name parent: self)
					to: self" ].

	(anRGBehavior hasResolvedName)
		ifTrue: [ self queryInterface behaviorsDictionary at: anRGBehavior name asSymbol put: anRGBehavior ]
]

{ #category : 'accessing - backend' }
RGEnvironment >> addGlobalVariable: anRGGlobalVariable [

	self backend forBehavior addGlobalVariable: anRGGlobalVariable to: self
]

{ #category : 'accessing - backend' }
RGEnvironment >> addPackage: anRGPackage [

	self backend forPackage addPackage: anRGPackage to: self.

	self announce: (PackageAdded to: anRGPackage)
]

{ #category : 'accessing' }
RGEnvironment >> announcer [

	^ announcer ifNil: [ announcer := RGEnvironmentAnnouncer new ]
]

{ #category : 'accessing' }
RGEnvironment >> ask [

	^ queryInterface ifNil: [ queryInterface := RGEnvironmentQueryInterface for: self ]
]

{ #category : 'accessing' }
RGEnvironment >> backend [

	^ backend ifNil: [ backend := RGEnvironmentBackend for: self ]
]

{ #category : 'accessing' }
RGEnvironment >> backend: anRGEnvironmentBackend [

	^ backend := anRGEnvironmentBackend
]

{ #category : 'accessing - backend' }
RGEnvironment >> behaviorsDo: aBlock [

	self backend forEnvironment behaviorsFor: self do: aBlock
]

{ #category : 'binding' }
RGEnvironment >> bindingOf: aSymbol [

	| behavior result |

	PseudoVariable lookupDictionary at: aSymbol ifPresent: [ :val | ^ val ].
	self globalVariablesBindings at: aSymbol ifPresent: [ :val | ^ val ].

	self ask globalVariables detect: [ :each | each name = aSymbol  ] ifFound: [ :found |
		self globalVariablesBindings at: aSymbol put: found.
		^ found ].

	behavior := self ask behaviorNamed: aSymbol.
	result := behavior ifNotNil: [ GlobalVariable key: aSymbol value: behavior].
	self globalVariablesBindings at: aSymbol put: result.
	^ result
]

{ #category : 'events' }
RGEnvironment >> child: aChildDefinition renamedFrom: oldName to: newName [

	self queryInterface behaviorsDictionary removeKey: oldName ifAbsent: [].
	self queryInterface behaviorsDictionary at: newName asSymbol put: aChildDefinition
]

{ #category : 'cleaning' }
RGEnvironment >> clean [

	self cleanWithMetaclassNamed: #Metaclass
]

{ #category : 'accessing - backend' }
RGEnvironment >> cleanBehaviors [

	| oldBehaviors |

	oldBehaviors := self ask behaviors.

	self backend forPackage cleanBehaviorsFor: self.

	oldBehaviors do: [ :each |
		self announcer behaviorRemoved: each ].

	self queryInterface resetBehaviorsDictionary
]

{ #category : 'accessing - backend' }
RGEnvironment >> cleanGlobalVariables [

	self backend forBehavior cleanGlobalVariablesFor: self
]

{ #category : 'accessing - backend' }
RGEnvironment >> cleanPackages [

	self backend forEnvironment cleanPackagesFor: self
]

{ #category : 'cleaning' }
RGEnvironment >> cleanSuperclassesOfMetaclasses [

	"fix suprclasses of metaclasses do not pointing to metaclasses of superclasses"

	self ask behaviorsDo: [ :each |
		(each isMetaclass and: [ each superclass isRingResolved not ])
			ifTrue: [
				each baseClass superclass ifNotNil: [
					each superclass: each baseClass superclass metaclass] ] ]
]

{ #category : 'cleaning' }
RGEnvironment >> cleanUnusedUnreferencedBehaviors [

	"remove behaviors that are not linked in the graph of the named behaviors"

	| referenced unreferenced newelyAdded |

	referenced := IdentitySet new.
	self ask behaviorsDo: [ :each |
		(each hasResolvedName) ifTrue: [
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

{ #category : 'cleaning' }
RGEnvironment >> cleanUnusedUnreferencedPackages [

	"remove packages that are not not used"

	| referenced unreferenced |

	referenced := IdentitySet new.
	self ask behaviorsDo: [ :each |
		referenced addAll: each referencedPackages ].

	unreferenced := (self ask packages copyWithoutAll: referenced).
	"Do not skip packages that only have extension methods..."
	unreferenced := unreferenced reject: [ :e | e definedBehaviors isEmpty and: [ e extensionMethods notEmpty ] ].

	unreferenced do: [:each | self removePackage: each  ].

	^ unreferenced
]

{ #category : 'cleaning' }
RGEnvironment >> cleanWithMetaclassNamed: aProposedName [

	self unifyMetaclass: aProposedName.
	self hasTraits ifTrue: [
		self unifyTrait.
		self unifyClassTrait. ].
	self cleanSuperclassesOfMetaclasses.
	self cleanUnusedUnreferencedBehaviors.
	self cleanUnusedUnreferencedPackages
]

{ #category : 'accessing - backend' }
RGEnvironment >> compileANewClassFrom: aString notifying: aMorph startingFrom: aClass [
	| importer |

	importer := RGChunkImporter new.
	importer environment: self environment.
	importer fileInFrom: (aString, '!') readStream.

	^ aClass
]

{ #category : 'utilities' }
RGEnvironment >> createDefaultEnvironment [

	| protoobject protoobjectClass object objectClass class classClass classDescription classDescriptionClass behavior behaviorClass metaclass metaclassClass kernelPackage |

	protoobject := RGClass unresolvedNamed: #'ProtoObject' withParent: self.
	protoobjectClass := RGMetaclass unresolvedNamed: #'ProtoObject class' withParent: self.
	object := RGClass unresolvedNamed: #'Object' withParent: self.
	objectClass := RGMetaclass unresolvedNamed: #'Object class' withParent: self.
	class := RGClass unresolvedNamed: #'Class' withParent: self.
	classClass := RGMetaclass unresolvedNamed: #'Class class' withParent: self.
	classDescription := RGClass unresolvedNamed: #'ClassDescription' withParent: self.
	classDescriptionClass := RGMetaclass unresolvedNamed: #'ClassDescription class' withParent: self.
	behavior := RGClass unresolvedNamed: #'Behavior' withParent: self.
	behaviorClass := RGMetaclass unresolvedNamed: #'Behavior class' withParent: self.
	metaclass := RGClass unresolvedNamed: #'Metaclass' withParent: self.
	metaclassClass := RGMetaclass unresolvedNamed: #'Metaclass class' withParent: self.
	kernelPackage := RGPackage unresolvedNamed: #'Kernel' withParent: self.

	{ protoobject. protoobjectClass. object. objectClass. class. classClass. classDescription. classDescriptionClass. behavior. behaviorClass. metaclass. metaclassClass } do: [ :each |
		kernelPackage pvtAddDefinedBehavior: each.
		self pvtAddBehavior: each.].
	self pvtAddPackage: kernelPackage.

	protoobject pvtSuperclass: protoobject.
	protoobject pvtMetaclass: protoobjectClass.
	protoobject behaviorStrategy pvtPackage: kernelPackage.

	protoobjectClass pvtSuperclass: class.
	protoobjectClass pvtMetaclass: metaclass.

	object pvtSuperclass: protoobject.
	object pvtMetaclass: objectClass.
	object behaviorStrategy pvtPackage: kernelPackage.

	objectClass pvtSuperclass: protoobjectClass.
	objectClass pvtMetaclass: metaclass.

	class pvtSuperclass: classDescription.
	class pvtMetaclass: classClass.
	class behaviorStrategy pvtPackage: kernelPackage.

	classClass pvtSuperclass: classDescriptionClass.
	classClass pvtMetaclass: metaclass.

	classDescription pvtSuperclass: behavior.
	classDescription pvtMetaclass: classDescriptionClass.
	classDescription behaviorStrategy pvtPackage: kernelPackage.

	classDescriptionClass pvtSuperclass: behaviorClass.
	classDescriptionClass pvtMetaclass: metaclass.

	behavior pvtSuperclass: object.
	behavior pvtMetaclass: behaviorClass.
	behavior behaviorStrategy pvtPackage: kernelPackage.

	behaviorClass pvtSuperclass: objectClass.
	behaviorClass pvtMetaclass: metaclass.

	metaclass pvtSuperclass: classDescription.
	metaclass pvtMetaclass: metaclassClass.
	metaclass behaviorStrategy pvtPackage: kernelPackage.

	metaclassClass pvtSuperclass: classDescriptionClass.
	metaclassClass pvtMetaclass: metaclass
]

{ #category : 'default model values' }
RGEnvironment >> defaultBehaviors [

	^ IdentitySet new
]

{ #category : 'default model values' }
RGEnvironment >> defaultGlobalVariables [

	^ IdentitySet new
]

{ #category : 'default model values' }
RGEnvironment >> defaultPackages [

	^ IdentitySet new
]

{ #category : 'default model values' }
RGEnvironment >> defaultVersion [

	^ 6
]

{ #category : 'accessing' }
RGEnvironment >> definitionFor: anObject [

	^ self backend definitionFor: anObject
]

{ #category : 'cleaning' }
RGEnvironment >> ensureClassNamed: aSymbol [

	self assert: (aSymbol endsWith: ' classTrait') not.

	^ self ask behaviorNamedExactlyAs: aSymbol ifAbsent: [
		 | behaviorClass newBehavior sibling siblingName |
		siblingName := (aSymbol endsWith: ' class')
			ifTrue: [ aSymbol withoutSuffix: ' class' ]
			ifFalse: [ (aSymbol, ' class') asSymbol ].

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
				ifFalse: [ RGClass ]
				ifTrue: [ RGMetaclass ].
			newBehavior := behaviorClass named: aSymbol parent: self.
			self addBehavior: newBehavior.
			newBehavior ]
]

{ #category : 'cleaning' }
RGEnvironment >> ensureClassTrait [

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

{ #category : 'cleaning' }
RGEnvironment >> ensureMetaclass [

	| aMetaclass |

	aMetaclass := self ask behaviorNamed: #Metaclass.
	aMetaclass ifNotNil: [ ^ aMetaclass ].

	aMetaclass := self ask behaviors detect: [ :each |
		(each propertyNamed: #Metaclass ifAbsent: [ false ]) ] ifNone: [ nil ].
	aMetaclass ifNotNil: [ ^ aMetaclass ].

	aMetaclass := self ensureClassNamed: #Metaclass.

	aMetaclass propertyNamed: #Metaclass put: true.

	^ aMetaclass
]

{ #category : 'cleaning' }
RGEnvironment >> ensureMetaclass: aProposedName [

	| aMetaclass |

	aMetaclass := self ask behaviorNamed: aProposedName.
	aMetaclass ifNotNil: [ ^ aMetaclass ].

	aMetaclass := self ask behaviors detect: [ :each |
		(each propertyNamed: #Metaclass ifAbsent: [ false ]) ] ifNone: [ nil ].
	aMetaclass ifNotNil: [ ^ aMetaclass ].

	aMetaclass := self ensureClassNamed: aProposedName.

	aMetaclass propertyNamed: #Metaclass put: true.

	^ aMetaclass
]

{ #category : 'cleaning' }
RGEnvironment >> ensureMetaclassTraitNamed: aSymbol [

	| found newTrait newMetaclassTrait similarMetaclass similarMetaclassName siblingName sibling originalMetaclass  |

	self assert: (aSymbol endsWith: ' class') not.
	self assert: (aSymbol endsWith: ' classTrait').

	found := self ask behaviorNamedExactlyAs: aSymbol.
	(found isNotNil and: [ found isMetaclassTrait ]) ifTrue: [ ^ found ].

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
							sibling convertToTrait.
							sibling classSide convertToMetaclassTrait.
							"sibling metaclass becomeForward: newMetaclassTrait.
							originalMetaclass becomeForward: newMetaclassTrait.
							sibling becomeForward: newTrait."
							"sibling classTrait: newMetaclassTrait.
							newMetaclassTrait baseTrait: sibling.
							"
							sibling classSide name: (siblingName, ' classTrait') asSymbol.
							sibling classSide propertyNamed: #resolved put: true.
							sibling classSide
							]]
				ifNil: [
					similarMetaclassName := ((aSymbol withoutSuffix: ' classTrait'), ' class') asSymbol.
					similarMetaclass := self ask behaviorNamed: similarMetaclassName.
					similarMetaclass
						ifNotNil: [
							"environment already contains metaclass of the similar name. It was probably
							created because correct type was not known. Convert it to classTrait"
							| similarClass |
							similarClass := similarMetaclass baseClass.
							newMetaclassTrait := similarMetaclass behaviorStrategy pvtAsMetaclassTrait.
							newTrait := similarClass convertToTrait.
							"similarMetaclass becomeForward: newMetaclassTrait.
							similarClass becomeForward: newTrait."
							newTrait classTrait: newMetaclassTrait.
							newMetaclassTrait baseTrait: newTrait.
							newTrait name: siblingName asSymbol.
							newMetaclassTrait ]
						ifNil: [
							"we need to create new behavior"
							| newBehavior |
							newBehavior := RGMetaclassTrait named: aSymbol parent: self.
							self addBehavior: newBehavior.
							newBehavior ] ] ]
		ifNotNil: [
			"some non-trait behavior found. We created this trait before as a class because correct behavior type was not known."
			newTrait := found convertToTrait.
			originalMetaclass := found metaclass.
			newMetaclassTrait := originalMetaclass pvtAsMetaclassTrait.
			self ask replaceName: originalMetaclass name with: newMetaclassTrait name.
			"found becomeForward: newTrait.
			found metaclass becomeForward: newMetaclassTrait.
			originalMetaclass becomeForward: newMetaclassTrait."
			found ]
]

{ #category : 'cleaning' }
RGEnvironment >> ensurePackageNamed: aSymbol [

	^ self ask packages
		detect: [:each | each name = aSymbol]
		ifNone: [ | newPackage |
				newPackage := RGPackage named: aSymbol parent: self.
				self addPackage: newPackage.
				newPackage ]
]

{ #category : 'cleaning' }
RGEnvironment >> ensureTrait [

	| aTrait |

	aTrait := self ask behaviorNamed: #Trait.
	aTrait ifNotNil: [ ^ aTrait ].

	aTrait := self ask behaviors detect: [ :each |
		(each propertyNamed: #Trait ifAbsent: [ false ]) ] ifNone: [ nil ].
	aTrait ifNotNil: [ ^ aTrait ].

	aTrait := self ensureClassNamed: #Trait.

	aTrait propertyNamed: #Trait put: true.

	^ aTrait
]

{ #category : 'cleaning' }
RGEnvironment >> ensureTraitNamed: aSymbol [

	| found siblingName sibling |

	self assert: (aSymbol endsWith: ' class') not.

	"process classTraits"
	(aSymbol endsWith: ' classTrait') ifTrue: [
		^ self ensureMetaclassTraitNamed: aSymbol ].

	"a trait with the same name already exists"
	found := self ask behaviorNamedExactlyAs: aSymbol.
	(found isNotNil and: [ found isTrait ]) ifTrue: [ ^ found ].

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
					newBehavior := RGTrait named: aSymbol parent: self.
					self addBehavior: newBehavior.
					newBehavior ] ]
		ifNotNil: [
			"some non-trait behavior found. We created this trait before as a class because correct behavior type was not known."
			found convertToTrait.
			found metaclass convertToMetaclassTrait.
			found ]
]

{ #category : 'unpackaged' }
RGEnvironment >> ensureUnpackagedPackage [

	^ self unpackagedPackageOrNil
		ifNotNil: [ :found | found ]
		ifNil: [ | newPackage |
			newPackage := RGPackage unresolvedWithParent: self.
			newPackage pvtName: self unpackagedPackageName.
			self pvtAddPackage: newPackage.
			^ newPackage ]
]

{ #category : 'accessing' }
RGEnvironment >> environment [

	^ self
]

{ #category : 'cleaning' }
RGEnvironment >> fixProtoObjectClassSuperclass [

	(self ask classNamed: #'ProtoObject class') superclass: (self ask classNamed: #Class)
]

{ #category : 'binding' }
RGEnvironment >> globalVariablesBindings [

	^ self propertyNamed: #globalVariablesBindings ifAbsentPut: [ IdentityDictionary new.]
]

{ #category : 'accessing - backend' }
RGEnvironment >> globalVariablesDo: aBlock [

	self backend forBehavior globalVariablesFor: self do: aBlock
]

{ #category : 'testing' }
RGEnvironment >> hasTraits [

	self behaviorsDo: [ :each |
		each isTrait ifTrue: [ ^ true ] ].

	^ false
]

{ #category : 'initialization' }
RGEnvironment >> initialize [

	super initialize.

	behaviors := self unresolvedValue: self defaultBehaviors.
	packages := self unresolvedValue: self defaultPackages.
	globalVariables := self unresolvedValue: self defaultGlobalVariables.

	version := self defaultVersion
]

{ #category : 'initialization' }
RGEnvironment >> initializeUnresolved [

	super initializeUnresolved.

	behaviors := self unresolvedValue: self defaultBehaviors.
	packages := self unresolvedValue: self defaultPackages.
	globalVariables := self unresolvedValue: self defaultGlobalVariables.

	version := self defaultVersion
]

{ #category : 'testing - types' }
RGEnvironment >> isEnvironment [

	^ true
]

{ #category : 'resolving' }
RGEnvironment >> makeResolved [

	super makeResolved.

	behaviors := self ask behaviors markAsRingResolved.
	packages := self ask packages markAsRingResolved.
	globalVariables := self ask globalVariables markAsRingResolved
]

{ #category : 'accessing' }
RGEnvironment >> package [

	^ nil
]

{ #category : 'accessing' }
RGEnvironment >> packages [
	^ packages
]

{ #category : 'accessing - backend' }
RGEnvironment >> packagesDo: aBlock [

	self backend  packagesFor: self do: aBlock
]

{ #category : 'private - backend interface' }
RGEnvironment >> pvtAddBehavior: anRGBehavior [

	self environment verifyOwnership: anRGBehavior.

	behaviors isRingResolved ifFalse: [
		self pvtCleanBehaviors ].

	(behaviors includes: anRGBehavior)
		ifFalse: [
			behaviors add: anRGBehavior.
			self announcer behaviorAdded: anRGBehavior]
]

{ #category : 'private - backend access' }
RGEnvironment >> pvtAddGlobalVariable: anRGGlobalVariable [

	self environment verifyOwnership: anRGGlobalVariable.

	globalVariables isRingResolved ifFalse: [
		self pvtCleanGlobalVariables  ].

	globalVariables add: anRGGlobalVariable
]

{ #category : 'private - backend interface' }
RGEnvironment >> pvtAddPackage: anRGPackage [

	self environment verifyOwnership: anRGPackage.

	packages isRingResolved ifFalse: [
		self pvtCleanPackages ].

	packages add: anRGPackage
]

{ #category : 'private - backend interface' }
RGEnvironment >> pvtBehaviorsDo: aBlock [

	^ behaviors value do: aBlock
]

{ #category : 'private - backend interface' }
RGEnvironment >> pvtCleanBehaviors [

	behaviors := self defaultBehaviors.

	"TODO:Announce if not empty"
]

{ #category : 'private - backend access' }
RGEnvironment >> pvtCleanGlobalVariables [

	globalVariables := self defaultGlobalVariables
]

{ #category : 'private - backend interface' }
RGEnvironment >> pvtCleanPackages [

	packages := self defaultPackages.

	"TODO:Announce if not empty"
]

{ #category : 'private - backend access' }
RGEnvironment >> pvtGlobalVariablesDo: aBlock [

	globalVariables value do: aBlock
]

{ #category : 'private - backend interface' }
RGEnvironment >> pvtPackagesDo: aBlock [

	^ packages value do: aBlock
]

{ #category : 'private - backend interface' }
RGEnvironment >> pvtRemoveBehavior: anRGBehavior [

	self verifyOwnership: anRGBehavior.

	behaviors remove: anRGBehavior.

	"TODO:Announce"
]

{ #category : 'private - backend access' }
RGEnvironment >> pvtRemoveGlobalVariable: anRGGlobalVariable [

	self environment verifyOwnership: anRGGlobalVariable.

	globalVariables remove: anRGGlobalVariable
]

{ #category : 'private - backend interface' }
RGEnvironment >> pvtRemovePackage: anRGPackage [

	self verifyOwnership: anRGPackage.

	packages remove: anRGPackage.

	"TODO:Announce"
]

{ #category : 'private - backend interface' }
RGEnvironment >> pvtResolvableProperties [

	^ super pvtResolvableProperties, {
		#behaviors -> behaviors.
		#packages -> packages.
		#globalVariables -> globalVariables.
	}
]

{ #category : 'accessing' }
RGEnvironment >> queryInterface [

	^ self ask
]

{ #category : 'accessing - backend' }
RGEnvironment >> removeBehavior: anRGBehavior [

	self backend forEnvironment removeBehavior: anRGBehavior from: self.

	"remove extensions methods deleted with the behavior from owning packages"
	anRGBehavior extensions do: [ :each |
		each package removeExtensionMethod: each ].

	(anRGBehavior package isNotNil and: [anRGBehavior package  definedBehaviors includes: anRGBehavior]) ifTrue: [
	anRGBehavior package removeDefinedBehavior: anRGBehavior.
	].

	self announcer behaviorRemoved: anRGBehavior.

	(anRGBehavior hasResolvedName) ifTrue: [
		self queryInterface behaviorsDictionary removeKey: anRGBehavior name ifAbsent: []]
]

{ #category : 'accessing - backend' }
RGEnvironment >> removeGlobalVariable: anRGGlobalVariable [

	self backend forBehavior removeGlobalVariable: anRGGlobalVariable from: self
]

{ #category : 'accessing - backend' }
RGEnvironment >> removePackage: anRGPackage [

	self backend forPackage removePackage: anRGPackage from: self.

	self announce: (PackageRemoved to: anRGPackage)
]

{ #category : 'unpackaged' }
RGEnvironment >> removeUnusedPackages [

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
			ifFalse: [ self removePackage: each ] ]
]

{ #category : 'cleaning' }
RGEnvironment >> unifyClassTrait [
	"set all metaclasses of classTraits to the same object (ClassTrait) "

	| aTrait |
	aTrait := self ensureClassTrait.

	self ask behaviorsDo: [ :each | each isMetaclassTrait ifTrue: [ each metaclass: aTrait ] ]
]

{ #category : 'cleaning' }
RGEnvironment >> unifyMetaclass [

	"set all metaclasses of metaclasses to the same object (Metaclass) "

	| aMetaclass |

	aMetaclass := self ensureMetaclass.

	self ask behaviorsDo: [ :each |
		(each isMetaclass)
			ifTrue: [ each metaclass: aMetaclass] ]
]

{ #category : 'cleaning' }
RGEnvironment >> unifyMetaclass: aProposedName [

	"set all metaclasses of metaclasses to the same object (Metaclass) "

	| aMetaclass |

	aMetaclass := self ensureMetaclass: aProposedName.

	self ask behaviorsDo: [ :each |
		(each isMetaclass)
			ifTrue: [ each metaclass: aMetaclass] ]
]

{ #category : 'cleaning' }
RGEnvironment >> unifyTrait [

	"set all metaclasses of metaclasses to the same object (Metaclass) "

	| aTrait |

	aTrait := self ensureTrait.

	self ask behaviorsDo: [ :each |
		(each isTrait)
			ifTrue: [ each metaclass: aTrait]].

	(self ask behaviors select: [ :each | each isRingResolved not and: [(each propertyNamed: #role) = #trait] ]) do: [:each | self removeBehavior: each  ]
]

{ #category : 'unpackaged' }
RGEnvironment >> unpackagedPackage [

	| aProtocol |
	aProtocol := RGPackage unresolvedWithParent: self.
	aProtocol pvtName: self unpackagedPackageName.

	^ aProtocol
]

{ #category : 'unpackaged' }
RGEnvironment >> unpackagedPackageName [

	^ '_UnpackagedPackage' asSymbol
]

{ #category : 'unpackaged' }
RGEnvironment >> unpackagedPackageOrNil [

	self packagesDo: [ :each |
		(each name = self unpackagedPackageName) ifTrue: [ ^ each ] ].

	^ nil
]

{ #category : 'utilities' }
RGEnvironment >> verifyOwnership: anRGObject [

	"ignore unresolved values. TODO: check default values ownership?"
	anRGObject isRingResolved ifFalse: [ ^ self.].

	(anRGObject environment = self)
		ifFalse: [ RGWrongEnvironment signal ]
]

{ #category : 'accessing' }
RGEnvironment >> version [
	^ version
]

{ #category : 'accessing' }
RGEnvironment >> version: anObject [
	version := anObject
]
