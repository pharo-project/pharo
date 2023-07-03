"
A RPackageOrganizer is responsible for providing all the package currently defined in the system.
In addition it provides a back pointer from the class to its package.

The classPackageMapping and the classExtendingPackageMapping should be moved in the future to the classes themselves. 


For tests or actions that could destroy the package organizer,  do not access directly the singleton of RPackageOrganizer.
Use instead 
		RPackage withOrganizer: aNewOrganizer do: ablock
			or via RPackage organizer
		
	
RPackageOrganizer fillUp will fill up the system from the current PackageOrganizer
	""self fillUp""
	
-----------------------------------------------------------------------------------------------------------------------------------------------	
	
	
A rpackageOrganizer update itself when some changes are made in the system. It does that by registering to a systemAnnoucer, specifying an action when an annoucement is triggered.
Here is what I (Cyrille Delaunay) propose to do for each annocuement triggered:

SystemCategoryAddedAnnouncement 	
	=> I would just register a new RPackage (if it does not already exist) in the RPackageOrganizer
	
SystemCategoryRemovedAnnouncement 
     => I would just unregister the RPackage concerned from the organizer

SystemCategoryRenamedAnnouncement
     => I would update the RPackage concerned, by changing its name
     => I would update the 'packages' dictionary of the organizer, putting the new name as key

SystemClassAddedAnnouncement 
    => Import the class in the RPackage concerned (RPackage >> importClass:)
    => Register the class in the 'classPackageMapping' dictionary of the organizer (RPackageOrganizer >> registerPackage:forClass)
    (=> maybe we should pay attention if both the class and the metaclass launch this kind of event ?)

SystemClassRecategorizedAnnouncement
    => I would update the old RPackage concerned:
            => unregister the class
            => unregister all defined methods of the class
    => I would update the new RPackage:
            => Import the class in the RPackage (importClass:)
    => I would update the organizer:
            => update the 'classPackageDictionary' to point on the new RPackage

 
SystemClassRemovedAnnouncement
    => I would update the RPackake concerned
             => unregister the class
             => unregister all defined methods of the class
    => I would update the organizer:
             => update the 'classPackageDictionary' to remove the class

SystemClassRenamedAnnouncement
    => I would update the RPackage in which the class is defined:
             => update the 'classDefinedSelectors' dictionary (replace the old key by the new one)
             => update the 'metaclassDefinedSelectors' dictionary (replace the old key by the new one)
    => I would update all RPackages extending this class
             => update the 'classExtensionsSelectors' dictionary (replace the old key by the new one)
             => update the 'metaclassclassExtensionsSelectors' dictionary (replace the old key by the new one)
    => I would update the organizer
             => update the 'classPackageDictionary' to replace the key with the new class name
             => update the 'classExtendingPackagesMapping' to replace the key with the new class name
                          
SystemClassReorganizedAnnouncement 
    (=> I guess we should check if extensions have not been added or removed ? 
      (to retrieve this information, the only thing I found is ClassDescription >> organization, and then check each category begining with '*' and compare with the organizer. seems to be painful, no?))
	=> when an extension is removed, all methods inside are removed. Therefore, the MethodRemovedAnnounecement will do the job. Not sur this one still usefull

SystemProtocolAddedAnnouncement
    => I don’t see anything to do for this annoucement

SystemProtocolRemovedAnnoucement
    => If the category is an extension from a package, I would move all the methods concerned, from the extending RPackage to the class RPackage

SystemMethodAddedAnnouncement
       => I would check the category in which the method has been defined
               => if it correspond to an extending package -> add the method to the extending RPackage
               => if not, add the method to the class parentPackage

SystemMethodModifiedAnnouncement
       this annoucement can correspond to several kind of modifications:
	       *  a method has been renamed
                       => I would update the rPackage in which the method is defined to replace the old selector by the new one
		* a method has been move to another category 
			-maybe from a classic category to an extending package
                             => we should move the method from the  method class parentPackage to extendingPackage package
			-maybe from an extending package to another extending package
                             => we should move the method from the  extendingPackage package to the other extendingPackage package
			-maybe from an extending package to a classic category
                             =>  we should move the method from the  extendingPackage to the method class parentPackage
		        -maybe from a classic category to another classic category
                             => we have nothing to do
			

SystemMethodRecategorizedAnnouncement
          same thing than above

SystemMethodRemovedAnnouncement
       => I would simply remove the method from the RPackage in which it is register
"
Class {
	#name : #RPackageOrganizer,
	#superclass : #Object,
	#instVars : [
		'classPackageMapping',
		'packages',
		'classExtendingPackagesMapping',
		'debuggingName',
		'environment',
		'categoryMap'
	],
	#category : #'RPackage-Core-Base'
}

{ #category : #singleton }
RPackageOrganizer class >> default [
	"Warning: Use 'self packageOrganizer' since I will be removed in the future."

	^ self packageOrganizer
]

{ #category : #'class initialization' }
RPackageOrganizer class >> initialize [
	"You need manually filll the data:

	self initializeDefaultFromMC.

	is the usual way (but is not in this package by default, it depends on MC)"
]

{ #category : #quieries }
RPackageOrganizer class >> isPackageDefinedForClass: aClass [

	^ (self default packageOf: aClass) notNil
]

{ #category : #private }
RPackageOrganizer class >> methodAdded: anEvent [
	"precondition: package exist, class exist"

	| methodCategory |
	methodCategory := anEvent protocol.
	(methodCategory isEmptyOrNil or: [ methodCategory first ~= $* ]) ifFalse: [
		(self isPackageDefinedForClass: anEvent methodClass) ifFalse: [
			RPackage new named: (self packageOrganizer categoryOfBehavior: anEvent methodClass instanceSide) ] ]
]

{ #category : #'class initialization' }
RPackageOrganizer class >> registerInterestToSystemAnnouncement [

	self packageOrganizer unregisterInterestToSystemAnnouncement.
	"To make sure that we do not have it twice registered"
	self packageOrganizer registerInterestToSystemAnnouncement
]

{ #category : #'class initialization' }
RPackageOrganizer class >> unregisterInterestToSystemAnnouncement [

	self default unregisterInterestToSystemAnnouncement
]

{ #category : #'deprecated - SystemOrganizer leftovers' }
RPackageOrganizer >> addCategory: catString [
	"Add a new category named catString"

	categoryMap at: catString asSymbol ifPresent: [ ^ self ] ifAbsentPut: [ OrderedCollection new ].

	SystemAnnouncer uniqueInstance classCategoryAdded: catString
]

{ #category : #private }
RPackageOrganizer >> addMethod: method [
	"we have to register the method in the parent RPackage of the class.
	to do that, we first have to look if the method is an extension from an external package:"

	| rPackage protocol |
	"If the class is not packaged yet, ignore the situation. This method is created during the creation of the class or on an anonymous class"
	method methodClass package name = RPackage defaultPackageName ifTrue: [ ^ self ].

	protocol := method protocolName.
	rPackage := (self hasPackageForProtocol: protocol)
		            ifTrue: [ self packageForProtocol: protocol inClass: method methodClass ]
		            ifFalse: [ self registerPackage: (RPackage named: (protocol copyWithout: $*) organizer: self) ].

	rPackage addMethod: method
]

{ #category : #'system integration' }
RPackageOrganizer >> announcer [
	^SystemAnnouncer uniqueInstance private
]

{ #category : #'initialization - data' }
RPackageOrganizer >> basicInitializeFromPackagesList: aPackagesList [

	aPackagesList
		do: [ :packageName | packages at: packageName asSymbol put: (RPackage named: packageName organizer: self) ]
		displayingProgress: 'Importing monticello packages'.

	Smalltalk allClassesAndTraits do: [ :behavior | self initializeFor: behavior ] displayingProgress: 'Importing behaviors'.

	Smalltalk allClassesAndTraits
		do: [ :behavior |
			self initializeMethodsFor: behavior.
			self initializeMethodsFor: behavior classSide ]
		displayingProgress: 'Importing methods'.

	Smalltalk allClassesAndTraits
		do: [ :behavior |
			behavior extensionProtocols do: [ :protocol | self initializeExtensionsFor: behavior protocol: protocol ].
			behavior classSide extensionProtocols do: [ :protocol | self initializeExtensionsFor: behavior classSide protocol: protocol ] ]
		displayingProgress: 'Importing extensions'
]

{ #category : #'private - registration' }
RPackageOrganizer >> basicRegisterPackage: aPackage [
	"A new package is now available and declared in the receiver. Note that it is a low level implementation method since it does not deal with package contained information and does not update the related mapping tables."

	^ packages at: aPackage name asSymbol put: aPackage
]

{ #category : #'private - registration' }
RPackageOrganizer >> basicUnregisterPackageNamed: aPackageName [
	"Unregister the specified package from the list of registered packages. Raise the RPackageUnregistered announcement. This is a low level action. It does not unregister the back pointer from classes to packages or any other information managed by the organizer"

	^ packages removeKey: aPackageName ifAbsent: [ self reportExtraRemovalOf: aPackageName ]
]

{ #category : #'deprecated - SystemOrganizer leftovers' }
RPackageOrganizer >> categories [

	^ categoryMap keys
]

{ #category : #'deprecated - SystemOrganizer leftovers' }
RPackageOrganizer >> categoriesMatching: matchString [
	"Return all matching categories"

	self categories ifNil: [ ^ #(  ) ].
	^ self categories select: [ :c | matchString match: c ]
]

{ #category : #'deprecated - SystemOrganizer leftovers' }
RPackageOrganizer >> categoryOfBehavior: behavior [
	"Answer the category associated with the argument. This method can take a Behavior or a Behavior name as parameter."

	| behaviorName |
	behaviorName := behavior isBehavior
		                ifTrue: [ behavior name ]
		                ifFalse: [ behavior ].

	categoryMap keysAndValuesDo: [ :category :classes | (classes includes: behaviorName) ifTrue: [ ^ category ] ].

	^ nil
]

{ #category : #'deprecated - SystemOrganizer leftovers' }
RPackageOrganizer >> categoryOfElement: behaviorName [

	self deprecated: 'Use #categoryOfBehavior: instead' transformWith: '`@rcv categoryOfElement: `@arg' -> '`@rcv categoryOfBehavior: `@arg'.
	^ self categoryOfBehavior: behaviorName
]

{ #category : #'private - registration' }
RPackageOrganizer >> checkPackageExistsOrRegister: packageName [

	(self packages anySatisfy: [ :package | packageName isCategoryOf: package packageName ]) ifFalse: [
		(RPackage named: packageName capitalized organizer: self) register ]
]

{ #category : #'deprecated - SystemOrganizer leftovers' }
RPackageOrganizer >> classesInCategory: category [

	^ (self listAtCategoryNamed: category) collect: [ :className | self environment at: className ]
]

{ #category : #'deprecated - SystemOrganizer leftovers' }
RPackageOrganizer >> classify: behaviorName under: categoryName [
	"Store the argument, element, in the category named heading."

	categoryName ifNil: [ self error: 'Category cannot be nil.' ].

	categoryMap
		at: categoryName
		ifPresent: [ :classes |
		(classes includes: behaviorName) ifTrue: [ ^ self ] ]
		ifAbsent: [ self addCategory: categoryName ].

	(self categoryOfBehavior: behaviorName) ifNotNil: [ :oldCategory |
		oldCategory = categoryName ifTrue: [ ^ self ].
		self removeBehavior: behaviorName ].

	(categoryMap at: categoryName) add: behaviorName
]

{ #category : #'deprecated - SystemOrganizer leftovers' }
RPackageOrganizer >> classifyAll: aCollection under: categoryName [

	aCollection do: [ :element | self classify: element under: categoryName ]
]

{ #category : #registration }
RPackageOrganizer >> createPackageNamed: aString [

	| instance |
	self validatePackageDoesNotExist: aString.

	instance := RPackage named: aString organizer: self.
	self registerPackage: instance.
	^ instance
]

{ #category : #'private - testing' }
RPackageOrganizer >> debuggingName [

	^ debuggingName
]

{ #category : #'private - testing' }
RPackageOrganizer >> debuggingName: aString [

	debuggingName := aString
]

{ #category : #initialization }
RPackageOrganizer >> defineUnpackagedClassesPackage [
	^ self ensureExistAndRegisterPackageNamed: RPackage defaultPackageName
]

{ #category : #'private - deprecated' }
RPackageOrganizer >> ensureExistAndRegisterPackageNamed: aSymbol [
	"DO NOT USE ME!!!
	
	Use #registerPackageNamed: instead.
	
	I am bad because I do not update the SystemOrganizer and I do not announce the new packages. I am still here only because I am still needed in the bootstrap. But please do not use me."

	^ (self includesPackageNamed: aSymbol)
		  ifFalse: [ self basicRegisterPackage: (RPackage named: aSymbol organizer: self) ]
		  ifTrue: [
			  | package |
			  package := self packageNamed: aSymbol.
			  package extendedClasses do: [ :extendedClass | self registerExtendingPackage: package forClass: extendedClass ].
			  package definedClasses do: [ :definedClass | self registerPackage: package forClass: definedClass ].
			  package ]
]

{ #category : #registration }
RPackageOrganizer >> ensureTagNamed: aTagName inPackageNamed: aPackageName [

	^ (self registerPackageNamed: aPackageName) addClassTag: aTagName
]

{ #category : #'deprecated - SystemOrganizer leftovers' }
RPackageOrganizer >> environment [
	 ^ environment ifNil: [ environment := Smalltalk globals]
]

{ #category : #'deprecated - SystemOrganizer leftovers' }
RPackageOrganizer >> environment: aSystemDictionary [
	 environment := aSystemDictionary
]

{ #category : #'package - access from class' }
RPackageOrganizer >> extendingPackagesOf: aClass [
	"Returns the packages extending the class aClass"
	^ classExtendingPackagesMapping at: aClass instanceSide name ifAbsent: [#()]
]

{ #category : #'package - access from class' }
RPackageOrganizer >> extendingPackagesOfClassNamed: aName [
	"Returns the packages extending the class named a Symbol"
	^ classExtendingPackagesMapping at: aName asSymbol ifAbsent: [#()]
]

{ #category : #'system integration' }
RPackageOrganizer >> fullyRemoveClassNamed: className [
	"Remove the class, the class backpointer, the extensions and the extension backPointer from the receiver and the class involved with the class named: className. className is a class name and should not be a metaclass one. "

	| rPackage |
	rPackage := (self packageOfClassNamed: className) ifNil: [ ^ self ].
	rPackage removeClassNamed: className.
	(self extendingPackagesOfClassNamed: className) do: [ :each | each removeAllMethodsFromClassNamed: className ]
]

{ #category : #'package - access from class' }
RPackageOrganizer >> globalPackageOf: aClass [
	"this method should return the 'global' parent package of aClass, that means the package holding the (possible) subcategory in which aClass is concretely defined. For example, 'Object package' returns Kernel-Object, whereas 'PackageOrganizer packageOf: Object' returns Kernel. So I guess that all use of 'packageOf' should be replaced by this method  "

	"RPackageOrganizer default globalPackageOf: Object"

	| classPackage |
	classPackage := self packageOf: aClass.
	^ self packages
		detect: [ :aRPackage | aRPackage ~= classPackage and: [ aRPackage systemCategories includes: classPackage name ] ]
		ifNone: [ classPackage ]
]

{ #category : #'system integration' }
RPackageOrganizer >> hasPackageExactlyMatchingExtensionName: anExtensionName [
	| extension |
	extension := anExtensionName asSymbol.
	"fast version"
	(self includesPackageNamed: extension)
		ifTrue: [  ^ true ].

	"slow version ignoring case"
	packages keysDo: [ :aSymbol |
		(aSymbol sameAs: extension)
			ifTrue: [ ^ true]].
	^ false
]

{ #category : #'system integration' }
RPackageOrganizer >> hasPackageForProtocol: aProtocolName [

	"If the protocol is not an extension protocol then we have the package since it's the same as the class."
	(aProtocolName beginsWith: '*') ifFalse: [ ^ true ].

	^ self hasPackageMatchingExtensionName: (aProtocolName copyWithout: $*)
]

{ #category : #'system integration' }
RPackageOrganizer >> hasPackageMatchingExtensionName: anExtensionName [

	(self hasPackageExactlyMatchingExtensionName: anExtensionName)
		ifTrue: [ ^true ].

	packages keysDo: [ :aSymbol |
		(anExtensionName beginsWith: aSymbol asString, '-' caseSensitive: false)
			ifTrue: [ ^ true]].
	^ false
]

{ #category : #testing }
RPackageOrganizer >> hasRegistered [
	"return true if this package organizer has already registered interest to system events and Monticello changes "

	^ self announcer hasSubscriber: self

	"|actionSequence|
	self announcer subscriptions ifNil: [^ true].
	actionSequence := self announcer subscriptions values
								detect: [:each | each anySatisfy: [:anAction | anAction receiver = RPackage organizer]] ifNone: [nil].
	self flag: #cyril.
	^ (actionSequence isNil not) and: [(MCWorkingCopy myDependents includes: self)]"
]

{ #category : #'deprecated - SystemOrganizer leftovers' }
RPackageOrganizer >> includesCategory: aString [
	"Tests if a category is already included."

	^ self categories
		  ifNil: [ false ]
		  ifNotNil: [ :categories | categories includes: aString ]
]

{ #category : #testing }
RPackageOrganizer >> includesPackage: aPackage [
	"Answer whether the receiver get a package as registered."
	^ self includesPackageNamed: aPackage name
]

{ #category : #'private - testing' }
RPackageOrganizer >> includesPackageBackPointerForClass: aClass [

	^ classPackageMapping includesKey: aClass instanceSide name asSymbol
]

{ #category : #testing }
RPackageOrganizer >> includesPackageNamed: aSymbol [
	"Answer whether the receiver get a package named aSymbol as registered."
	^ packages includesKey:  aSymbol asSymbol
]

{ #category : #initialization }
RPackageOrganizer >> initialize [

	super initialize.

	packages := IdentityDictionary new.
	classPackageMapping := IdentityDictionary new.
	classExtendingPackagesMapping := IdentityDictionary new.
	debuggingName := ''.
	categoryMap := Dictionary new.

	self defineUnpackagedClassesPackage
]

{ #category : #'initialization - data' }
RPackageOrganizer >> initializeExtensionsFor: aBehavior protocol: aProtocol [
	| package protocolName nonTraitMethods |

	protocolName := (aProtocol name allButFirst) trimBoth.
	package := (self packageMatchingExtensionName: protocolName) ifNil: [ self basicRegisterPackage: (RPackage named: protocolName organizer: self) ].
	nonTraitMethods := aProtocol methodSelectors
		select: [ :eachSelector | (aBehavior >> eachSelector) origin = aBehavior ].
	nonTraitMethods ifEmpty:[^ self].
	self registerExtendingPackage: package forClass: aBehavior.
	nonTraitMethods
		do: [ :eachSelector | package addMethod: aBehavior >> eachSelector ]
]

{ #category : #'initialization - data' }
RPackageOrganizer >> initializeFor: aBehavior [
	| package |

	package := self packageMatchingExtensionName: aBehavior category.
	package ifNil: [
		"It should not happen.
		 But actually could happen that one class is in a SystemCategory and not in a MC"
		package := self basicRegisterPackage: (RPackage named: aBehavior category organizer: self) ].
	package addClassDefinition: aBehavior.
	package
		addClassDefinition: aBehavior
		toClassTag: aBehavior category asSymbol.
	self registerPackage: package forClass: aBehavior
]

{ #category : #'initialization - data' }
RPackageOrganizer >> initializeMethodsFor: aBehavior [

	| package |
	package := aBehavior package.
	(aBehavior protocols
		reject: [ :each | each isExtensionProtocol ])
		do: [ :protocol |
			 (protocol methodSelectors
				select: [ :eachSelector | (aBehavior >> eachSelector) origin = aBehavior ])
				do: [ :eachSelector | package addMethod: (aBehavior >> eachSelector) ] ]
]

{ #category : #'deprecated - SystemOrganizer leftovers' }
RPackageOrganizer >> isEmptyCategoryNamed: categoryName [

	^ categoryMap
		  at: categoryName
		  ifPresent: [ :classes | classes isEmpty ]
		  ifAbsent: [ false ]
]

{ #category : #'deprecated - SystemOrganizer leftovers' }
RPackageOrganizer >> listAtCategoryNamed: categoryName [
	"Answer the array of elements associated with the name, categoryName."

	^ (categoryMap at: categoryName ifAbsent: [ Array new ]) asArray
]

{ #category : #'deprecated - SystemOrganizer leftovers' }
RPackageOrganizer >> orderedTraitsIn: category [
	"Answer an OrderedCollection containing references to the traits in the
	category whose name is the argument, category (a string). The traits
	are ordered so they can be filed in."

	| behaviors traits |
	behaviors := (self listAtCategoryNamed: category) collect: [ :title | self environment at: title ].
	traits := behaviors select: [ :each | each isTrait ].
	traits := traits asSortedCollection: [ :t1 :t2 | (t2 traitComposition allTraits includes: t1) or: [ (t1 traitComposition allTraits includes: t2) not ] ].
	^ traits asArray
]

{ #category : #accessing }
RPackageOrganizer >> packageDefiningOrExtendingSelector: aSelector inClassNamed: aClassNameSymbol [
	"this implementation is slower
		aClass packages detect: [:each | each includesSelector: aSelector ofClass: aClass ]"
	| classPackage |

	classPackage := self packageOfClassNamed: aClassNameSymbol.
	classPackage ifNil: [ ^ nil ].
	(classPackage includesSelector: aSelector ofClassName: aClassNameSymbol)
		ifTrue: [ ^classPackage ].

	^(self extendingPackagesOfClassNamed: aClassNameSymbol)
		detect: [ :p | p includesSelector: aSelector ofClassName: aClassNameSymbol ]
		ifNone: [ nil ]
]

{ #category : #accessing }
RPackageOrganizer >> packageDefiningOrExtendingSelector: aSelector inMetaclassNamed: aClassNameSymbol [
	"this implementation is slower
		aClass packages detect: [:each | each includesSelector: aSelector ofClass: aClass ]"
	| classPackage |

	classPackage := self packageOfClassNamed: aClassNameSymbol.
	classPackage ifNil: [ ^ nil ].
	(classPackage includesSelector: aSelector ofMetaclassName: aClassNameSymbol)
		ifTrue: [ ^classPackage ].

	^(self extendingPackagesOfClassNamed: aClassNameSymbol)
		detect: [ :p | p includesSelector: aSelector ofMetaclassName: aClassNameSymbol ]
		ifNone: [ nil ]
]

{ #category : #'system integration' }
RPackageOrganizer >> packageForProtocol: aProtocolName inClass: aClass [
	"According aProtocolName is an extension protocol, will look for a matching package. If no matching package is found ,  return nil. If aProtocolName is not an extension protocol, return the parent package of aClass"

	|tmpProtocol|
	tmpProtocol := aProtocolName ifNil: [''].
	^ (tmpProtocol beginsWith: '*')
		ifTrue: [  (self packageMatchingExtensionName: (tmpProtocol copyWithout: $*)) ]
		ifFalse: [ aClass package ]
]

{ #category : #'system integration' }
RPackageOrganizer >> packageMatchingExtensionName: anExtensionName [

	"return nil if no package is not found"
	| tmpPackageName |

	"we first look if their is a package matching exactly the name specified"
	(self packageNamedIgnoreCase: anExtensionName ifAbsent: [ nil ]) ifNotNil: [ :package | ^ package ].

	"if no package was found, we try to find one matching the begining of the name specified"
	tmpPackageName := ''.
	packages keysDo: [:aSymbol |
		(anExtensionName beginsWith: (aSymbol asString, '-') caseSensitive: false)
			ifTrue: [
				"we keep the longest package name found"
				(aSymbol size > tmpPackageName size)
					ifTrue: [ tmpPackageName := aSymbol ]]].

	^ tmpPackageName = ''
		ifTrue: [ nil ]
		ifFalse: [ self packageNamed: tmpPackageName ]
]

{ #category : #'system integration' }
RPackageOrganizer >> packageMatchingExtensionName: aString includingClass: aClass [
	| package categoryName |

	categoryName := aString.
	package := self packageMatchingExtensionName: categoryName.
	[ package includesClass: aClass ]
		whileFalse: [
			(categoryName includes: $-)
				ifFalse: [ self error: 'Class not categorized!' ].

			categoryName := categoryName copyUpToLast: $-.
			package := self packageMatchingExtensionName: categoryName ].

	^ package
]

{ #category : #accessing }
RPackageOrganizer >> packageNamed: aSymbol [
	^ self
		packageNamed: aSymbol
		ifAbsent: [ KeyNotFound signalFor: aSymbol ]
]

{ #category : #accessing }
RPackageOrganizer >> packageNamed: aSymbol  ifAbsent: errorBlock [
	^ packages
		at: aSymbol asSymbol
		ifAbsent: errorBlock
]

{ #category : #private }
RPackageOrganizer >> packageNamedIgnoreCase: aSymbol ifAbsent: aBlock [
	"In case of extensions, I can need to take a package ignoring name"
	self packagesDo: [  :each |
		(each name sameAs: aSymbol)
			ifTrue: [  ^ each  ]  ].

	^ aBlock value
]

{ #category : #'package - names-cache' }
RPackageOrganizer >> packageNames [

	^ packages keys
]

{ #category : #'package - names-cache' }
RPackageOrganizer >> packageNamesDo: aBlock [

	^ self packageNames do: aBlock
]

{ #category : #'package - access from class' }
RPackageOrganizer >> packageOf: aClass [

	^ classPackageMapping
		at: aClass instanceSide originalName
		ifAbsent: [self packageNamed: RPackage defaultPackageName]
]

{ #category : #'package - access from class' }
RPackageOrganizer >> packageOfClassNamed: aName [

	self flag: #pharoFixMe. "Should probably return _UnunpackagedPackage instead of nil"
	^ classPackageMapping at: aName asSymbol ifAbsent: [nil]
]

{ #category : #accessing }
RPackageOrganizer >> packages [

	^ packages values
]

{ #category : #accessing }
RPackageOrganizer >> packagesDo: aBlock [

	packages valuesDo: aBlock
]

{ #category : #printing }
RPackageOrganizer >> printOn: aStream [

	super printOn: aStream.
	debuggingName ifNotNil: [aStream nextPutAll: debuggingName ]
]

{ #category : #initialization }
RPackageOrganizer >> register [

	"self registerInterestToMC."
	self registerInterestToSystemAnnouncement
]

{ #category : #'private - registration' }
RPackageOrganizer >> registerExtendingPackage: aPackage forClass: aClass [

	| cur |

	cur := classExtendingPackagesMapping at: aClass instanceSide name ifAbsent: [ nil ].
	cur
		ifNil: [ classExtendingPackagesMapping at: aClass instanceSide name put: ( Set with: aPackage ) ]
		ifNotNil: [ cur add: aPackage ]
]

{ #category : #'private - registration' }
RPackageOrganizer >> registerExtendingPackage: aPackage forClassName: aClassName [

	| cur |

	cur := classExtendingPackagesMapping at: aClassName asSymbol ifAbsent: [ nil ].
	cur
		ifNil: [ classExtendingPackagesMapping at: aClassName asSymbol put: ( Set with: aPackage ) ]
		ifNotNil: [ cur add: aPackage ]
]

{ #category : #'system integration' }
RPackageOrganizer >> registerInterestToAnnouncer: anAnnouncer [
	"There should be only one"

	anAnnouncer unsubscribe: self.

	anAnnouncer weak
		when: CategoryAdded send: #systemCategoryAddedActionFrom: to: self;
		when: CategoryRemoved send: #systemCategoryRemovedActionFrom: to: self;
		when: CategoryRenamed send: #systemCategoryRenamedActionFrom: to: self;
		when: ClassAdded send: #systemClassAddedActionFrom: to: self;
		when: ClassRecategorized send: #systemClassRecategorizedActionFrom: to: self;
		when: ClassRemoved send: #systemClassRemovedActionFrom: to: self;
		when: ClassRenamed send: #systemClassRenamedActionFrom: to: self;
		when: ClassReorganized send: #systemClassReorganizedActionFrom: to: self;
		when: MethodAdded send: #systemMethodAddedActionFrom: to: self;
		when: MethodModified send: #systemMethodModifiedActionFrom: to: self;
		when: MethodRecategorized send: #systemMethodRecategorizedActionFrom: to: self;
		when: MethodRemoved send: #systemMethodRemovedActionFrom: to: self
]

{ #category : #'system integration' }
RPackageOrganizer >> registerInterestToSystemAnnouncement [
	"self registerInterestToSystemAnnouncement"
	<systemEventRegistration>

	self registerInterestToAnnouncer: self announcer
]

{ #category : #registration }
RPackageOrganizer >> registerPackage: aPackage [
	"A new package is now available and declared in the receiver. "

	self validateCanBeAddedPackage: aPackage.

	self basicRegisterPackage: aPackage.
	aPackage extendedClasses
		do: [ :extendedClass | self registerExtendingPackage: aPackage forClass: extendedClass].
	aPackage definedClasses
		do: [ :definedClass | self registerPackage: aPackage forClass: definedClass].

	SystemAnnouncer announce: (PackageAdded to: aPackage).

	^ aPackage
]

{ #category : #'private - registration' }
RPackageOrganizer >> registerPackage: aPackage forClass: aClass [
	(aPackage includesClass: aClass)
		ifFalse:
			[self error: aPackage name , ' does not includes the class ' , aClass name].
	^classPackageMapping at: aClass instanceSide name put: aPackage
]

{ #category : #'private - registration' }
RPackageOrganizer >> registerPackage: aPackage forClassName: aClassNameSymbol [
	"Register aPackage as the package of the class aClassNameSymbol. The package should contain the class in its class definitions."

	(aPackage includesClassNamed: aClassNameSymbol asSymbol)
		ifFalse:
			[self error: aPackage name , ' does not include the class ' , aClassNameSymbol].
	^classPackageMapping at: aClassNameSymbol put: aPackage
]

{ #category : #registration }
RPackageOrganizer >> registerPackageNamed: aString [

	^ self packageNamed: aString asSymbol ifAbsent: [ self registerPackage: (RPackage named: aString organizer: self) ]
]

{ #category : #'deprecated - SystemOrganizer leftovers' }
RPackageOrganizer >> removeBehavior: behavior [
	"Remove the behavior from all categories. I can take a Behavior or a Behavior name as argument."

	| behaviorName |
	behaviorName := behavior isBehavior
		                ifTrue: [ behavior name ]
		                ifFalse: [ behavior ].

	categoryMap keysAndValuesDo: [ :category :classes | (classes includes: behaviorName) ifTrue: [ classes remove: behaviorName ] ]
]

{ #category : #'deprecated - SystemOrganizer leftovers' }
RPackageOrganizer >> removeCategoriesMatching: matchString [
	"Remove all matching categories with their classes"

	(self categoriesMatching: matchString) do: [ :c | self removeSystemCategory: c ]
]

{ #category : #'deprecated - SystemOrganizer leftovers' }
RPackageOrganizer >> removeCategory: category [
	"Remove the category named, cat. Create an error notificiation if the
	category has any elements in it."

	categoryMap
		at: category
		ifPresent: [ :classes | classes ifNotEmpty: [ ^ self error: 'Cannot remove non-empty category ' , category , '. Present classes: ' , classes printString ] ]
		ifAbsent: [ ^ self ].

	categoryMap removeKey: category.

	SystemAnnouncer uniqueInstance classCategoryRemoved: category
]

{ #category : #'deprecated - SystemOrganizer leftovers' }
RPackageOrganizer >> removeElement: behaviorName [

	self deprecated: 'Use #removeBehavior: instead' transformWith: '`@rcv removeElement: `@arg' -> '`@rcv removeBehavior: `@arg'.
	^ self removeBehavior: behaviorName
]

{ #category : #'deprecated - SystemOrganizer leftovers' }
RPackageOrganizer >> removeEmptyPackages [
	"Remove empty packages."

	(categoryMap select: [ :classes | classes isEmpty ]) keys
		ifNotEmpty: [ :emptyPackages |
			emptyPackages do: [ :emptyPackage | categoryMap removeKey: emptyPackage ] ]
]

{ #category : #'deprecated - SystemOrganizer leftovers' }
RPackageOrganizer >> removeSystemCategory: category [
	"remove all the classes and traits associated with the category"

	(self orderedTraitsIn: category) , (self superclassOrder: category) reverseDo: [ :each | each removeFromSystem ].

	self removeCategory: category
]

{ #category : #'deprecated - SystemOrganizer leftovers' }
RPackageOrganizer >> renameCategory: oldCatString toBe: newCatString [
	"Rename a category. No action if new name already exists, or if old name does not exist."

	categoryMap at: newCatString ifPresent: [ "new name exists, so no action" ^ self ].

	categoryMap
		at: oldCatString
		ifPresent: [ :classes |
			categoryMap at: newCatString put: classes.
			categoryMap removeKey: oldCatString ]
		ifAbsent: [ "old name not found, so no action" ^ self ].

	SystemAnnouncer uniqueInstance classCategoryRenamedFrom: oldCatString to: newCatString
]

{ #category : #'system integration' }
RPackageOrganizer >> renamePackage: rPackage from: oldName to: newName [

	| classesAndProtocolsToRename |
	rPackage name: newName.

	packages at: newName put: rPackage.
	packages removeKey: oldName ifAbsent: [ self reportBogusBehaviorOf: #systemCategoryRenamedActionFrom: ].

	"we also rename all the extension protocols in the system with the new name"
	classesAndProtocolsToRename := rPackage extensionMethods asIdentitySet.
	classesAndProtocolsToRename do: [ :method | method methodClass renameProtocol: method protocol as: '*' , newName ]
]

{ #category : #private }
RPackageOrganizer >> reportBogusBehaviorOf: aSelector [

	self traceCr: 'RPackage log: Something wrong around ', aSelector asString , 'since the removeKey: is called on not present information.'
]

{ #category : #private }
RPackageOrganizer >> reportExtraRemovalOf: aPackageOrClass [

	self traceCr: 'The class ' , aPackageOrClass printString , ' is removed twice'
]

{ #category : #registration }
RPackageOrganizer >> signalPackageExists: aPackageName [

	RPackageConflictError signal: ('A package named {1} already exists' format: {aPackageName})
]

{ #category : #private }
RPackageOrganizer >> stopNotification [
	"(self instVarNamed: #default) stopNotification"

	"pay attention that we can break the system using this method"

	SystemAnnouncer uniqueInstance unsubscribe: self.

	self class environment at: #MCWorkingCopy ifPresent: [:wc |
		wc removeDependent: self]
]

{ #category : #'deprecated - SystemOrganizer leftovers' }
RPackageOrganizer >> superclassOrder: category [
	"Answer an OrderedCollection containing references to the classes in the
	category whose name is the argument, category (a string). The classes
	are ordered with superclasses first so they can be filed in."

	| behaviors classes |
	behaviors := (self listAtCategoryNamed: category) collect: [ :title | self environment at: title ].
	classes := behaviors select: [ :each | each isBehavior ].
	^ Class superclassOrder: classes
]

{ #category : #'system integration' }
RPackageOrganizer >> systemCategoryAddedActionFrom: ann [

	| package |
	package := (self packageMatchingExtensionName: ann categoryName asString) ifNil: [
		           self registerPackage: (RPackage named: ann categoryName asSymbol organizer: self) ].
	package addClassTag: ann categoryName asSymbol
]

{ #category : #'system integration' }
RPackageOrganizer >> systemCategoryRemovedActionFrom: ann [
	"When a system category is removed, we may: remove a tag, or remove a rpackage. If we remove a RPackage, unregister the linked MCWorkingCopy. If it is a tag, do nothing? (from what I know of RPackage, the tag should already have disappeared because it would have been empty)."

	| categoryName |
	categoryName := ann categoryName asSymbol.

	(self packageMatchingExtensionName: categoryName) ifNotNil: [ :rPackage |
		rPackage name = categoryName
			ifTrue: [ self unregisterPackage: rPackage ]
			ifFalse: [ rPackage classTagNamed: (categoryName withoutPrefix: rPackage name , '-') ifPresent: [ :tag | rPackage removeClassTag: tag name ] ] ]
]

{ #category : #'system integration' }
RPackageOrganizer >> systemCategoryRenamedActionFrom: ann [
	| package oldName newName |

	oldName := ann oldCategoryName asSymbol.
	newName := ann newCategoryName asSymbol.
	package := (self packageMatchingExtensionName: ann oldCategoryName) ifNil: [ (RPackage named: newName organizer: self) register ].
	package name = ann oldCategoryName ifTrue: [
		self
			renamePackage: package
			from: oldName
			to: newName ].

	package
		classTagNamed: oldName
		ifPresent: [ :tag | tag basicRenameTo: newName ]
]

{ #category : #'system integration' }
RPackageOrganizer >> systemClassAddedActionFrom: ann [
	| class package categoryNameSymbol |

	class := ann classAffected.

	categoryNameSymbol := class category.
	package := (self packageMatchingExtensionName: categoryNameSymbol)
		ifNil: [ self registerPackage: (RPackage named: categoryNameSymbol organizer: self) ].

	package importClass: class
]

{ #category : #'system integration' }
RPackageOrganizer >> systemClassRecategorizedActionFrom: announcement [

	| class newPackage newPackageTag oldPackage newPackageName oldPackageName |
	class := announcement classAffected.

	newPackageName := announcement newCategory.
	oldPackageName := announcement oldCategory.
	newPackageName = oldPackageName ifTrue: [ ^ self ].
	oldPackage := self packageMatchingExtensionName: oldPackageName includingClass: class.
	newPackage := (self packageMatchingExtensionName: newPackageName) ifNil: [ self registerPackage: (RPackage named: newPackageName organizer: self) ].

	newPackageTag := newPackage addClassTag: newPackageName.

	newPackage moveClass: class fromPackage: oldPackage toTag: newPackageTag
]

{ #category : #'system integration' }
RPackageOrganizer >> systemClassRemovedActionFrom: ann [
	"A class has been removed, we should update the package adequately."

	| class className |
	class := ann classRemoved.
	className  := class originalName.

	self fullyRemoveClassNamed: className
]

{ #category : #'system integration' }
RPackageOrganizer >> systemClassRenamedActionFrom: ann [
	"When a class is renamed, we update its package as well as the organizer"

	| newName oldName class rPackage extendingRPackages packageName |
	class := ann classAffected.
	packageName := ann category.
	oldName := ann oldName.
	newName := ann newName.
	rPackage := self packageOfClassNamed: oldName.
	extendingRPackages := self extendingPackagesOfClassNamed: oldName.

	rPackage updateDefinedClassNamed: oldName withNewName: newName.
	"we have to update all RPackages extending this class"
	extendingRPackages do: [:aRPackage |
		aRPackage updateExtensionForClassNamed: oldName withNewName: newName].

	"we have to update the RPackageOrganizer.
	update the 'classPackageDictionary' to replace the key with the new class name"
	self unregisterPackage: rPackage forClassName: oldName.
	self registerPackage: rPackage forClassName: newName.

	"update the 'classExtendingPackagesMapping' to replace the key with the new class name"
	extendingRPackages do: [:aRPackage |
		self unregisterExtendingPackage: aRPackage forClassName: oldName.
		self registerExtendingPackage:  aRPackage forClassName: newName
		].
	self flag: #cyrille. "we will see that."
	"maybe we should remove also the key from the extendedPackages dictionary"
	"CD: This is what are doing the lines above I think"
]

{ #category : #'system integration' }
RPackageOrganizer >> systemClassReorganizedActionFrom: ann [
	"when a class is reorganized, we have to check if an extension has not been added"

	ann classReorganized extensionProtocols do: [ :protocol | self checkPackageExistsOrRegister: protocol name allButFirst ]
]

{ #category : #'system integration' }
RPackageOrganizer >> systemMethodAddedActionFrom: ann [
	| method |

	"If the method origin is not the one of the event, we do not care about that method"

	method := ann method.
	method origin = ann methodClass
		ifFalse: [ ^ self ].

	self addMethod: method
]

{ #category : #'system integration' }
RPackageOrganizer >> systemMethodModifiedActionFrom: ann [

	"this announcement can correspond to several kind of modifications:
		=> a method has been renamed
		=> a method has been move to another category // SHOULD BE TREATED WITH SystemMethodRecategorizedEvent
		(
			-maybe from the classic category to an extending package
			-maybe from an extending package to another extending package
			-maybe from an extending package to a classic category
			- maybe from a classic category to another classic category
			)
	"
	| oldMethod newMethod methodPackage |
	newMethod := ann newMethod.
	oldMethod := ann oldMethod.
	self flag: #cyrille.
	"This is not clear that we need to do soemthing here since the method is removed and added so the other events should be handled correctly
	We should remove this method to see if this is working."

	"If the method origin is not the one of the event, we do not care about that method"
	newMethod origin = ann methodClass ifFalse: [^ self].


	"Special case for trait methods added an override"
	newMethod origin = oldMethod origin
		ifFalse: [ ^ self addMethod: newMethod ].

	methodPackage := newMethod packageFromOrganizer: self.
	"maybe the name of the method has changed"
	oldMethod selector = newMethod selector
		ifFalse: [ methodPackage updateSelector: oldMethod selector inClass: oldMethod methodClass withNewSelector: newMethod selector]
]

{ #category : #'system integration' }
RPackageOrganizer >> systemMethodRecategorizedActionFrom: ann [
	"a protocol has been renamed
	a method has been removed. // WILL BE TREATED WITH SystemMethodRemovedAnnouncement
	a method has been moved to another category
		-maybe from the classic category to an extending package
		-maybe from an extending package to another extending package
		-maybe from an extending package to a classic category
		- maybe from a classic category to another classic category.
	Get out of here if my organizer has no package for the class! (Anonymous classes)"

	| oldProtocol newProtocol method methodPackage destinationPackage |
	method := ann method ifNil: [ ^ self ].
	method origin = ann methodClass ifFalse: [ ^ self ].
	(self packageOfClassNamed: ann methodClass instanceSide name) ifNil: [ ^ self ].

	newProtocol := ann newProtocol ifNotNil: [ :protocol | protocol name ] ifNil: [ ^ self ].

	oldProtocol := ann oldProtocol ifNotNil: [ :protocol | protocol name ] ifNil: [ '' ].

	methodPackage := method packageFromOrganizer: self.
	newProtocol asLowercase = oldProtocol asLowercase ifTrue: [ ^ self ].

	destinationPackage := (self hasPackageForProtocol: newProtocol)
		                      ifTrue: [ self packageForProtocol: newProtocol inClass: method methodClass ]
		                      ifFalse: [
			                      (newProtocol beginsWith: '*')
				                      ifTrue: [ self ensureExistAndRegisterPackageNamed: newProtocol allButFirst capitalized ]
				                      ifFalse: [ method methodClass package ] ].

	methodPackage := (self hasPackageForProtocol: oldProtocol)
		                 ifTrue: [ self packageForProtocol: oldProtocol inClass: method methodClass ]
		                 ifFalse: [ method methodClass package ].

	methodPackage = destinationPackage ifTrue: [ ^ self ].

	(methodPackage methods includes: method) ifTrue: [ methodPackage removeMethod: method ].

	destinationPackage addMethod: method.

	SystemAnnouncer uniqueInstance methodRepackaged: method from: methodPackage to: destinationPackage
]

{ #category : #'system integration' }
RPackageOrganizer >> systemMethodRemovedActionFrom: ann [

	| method methodPackage |
	method := ann method.
	"If the method is provided by a trait, we do not care about it"
	ann isProvidedByATrait ifTrue: [^ self].

	ann methodClass isMeta
		ifFalse: [
			methodPackage := self packageDefiningOrExtendingSelector: ann selector inClassNamed: ann methodClass instanceSide originalName.
			methodPackage ifNotNil: [
				methodPackage removeSelector: ann selector ofClassName: ann methodClass instanceSide originalName].
		]
		ifTrue: [
			methodPackage := self packageDefiningOrExtendingSelector: ann selector inMetaclassNamed: ann methodClass instanceSide originalName.
			methodPackage ifNotNil: [
				methodPackage removeSelector: ann selector ofMetaclassName: ann methodClass instanceSide originalName].
		]
]

{ #category : #accessing }
RPackageOrganizer >> testPackageNames [

	^ packages keys select: [:nameSymbol | (self packageNamed: nameSymbol) isTestPackage]
]

{ #category : #accessing }
RPackageOrganizer >> testPackages [

	^ packages values select: #isTestPackage
]

{ #category : #initialization }
RPackageOrganizer >> unregister [
	SystemAnnouncer uniqueInstance unsubscribe: self
]

{ #category : #'private - registration' }
RPackageOrganizer >> unregisterExtendingPackage: aPackage forClass: aClass [
	| extendingPackageForClass |

	extendingPackageForClass := classExtendingPackagesMapping
		at: aClass instanceSide name
		ifAbsent: [ nil ].
	extendingPackageForClass ifNotNil: [
		extendingPackageForClass
			remove: aPackage
			ifAbsent: [] "not happy with this one" ]
]

{ #category : #'private - registration' }
RPackageOrganizer >> unregisterExtendingPackage: aPackage forClassName: aClassName [

	| cur |

	cur := classExtendingPackagesMapping at: aClassName asSymbol ifAbsent: [ nil ].
	cur ifNotNil: [ cur remove: aPackage ifAbsent: [ "not happy with this one" ] ]
]

{ #category : #'system integration' }
RPackageOrganizer >> unregisterInterestToSystemAnnouncement [
	"self unregisterInterestToSystemAnnouncement"

	SystemAnnouncer uniqueInstance unsubscribe: self
]

{ #category : #registration }
RPackageOrganizer >> unregisterPackage: aPackage [
	"Unregister the specified package from the list of registered packages. Raise the announcement RPackageUnregistered."

	self basicUnregisterPackageNamed: aPackage name.
	aPackage extendedClasses
		do: [ :extendedClass | self unregisterExtendingPackage: aPackage forClass: extendedClass].
	aPackage definedClasses
		do: [ :definedClass | self unregisterPackage: aPackage forClass: definedClass].
	SystemAnnouncer announce: (PackageRemoved to: aPackage).

	^ aPackage
]

{ #category : #'private - registration' }
RPackageOrganizer >> unregisterPackage: aPackage forClass: aClass [
	"unregister the back pointer mapping from classes to packages."

	^classPackageMapping
		removeKey: aClass instanceSide name
		ifAbsent: [ self reportExtraRemovalOf: aClass name ]
]

{ #category : #'private - registration' }
RPackageOrganizer >> unregisterPackage: aPackage forClassName: aClassName [
	"Unregister the package back pointer for a given class. The class should not belong to the package anymore before removing the back pointer."

	(aPackage includesClassNamed: aClassName asSymbol)
		ifTrue:
			[self error: aPackage name , ' still includes the class ' , aClassName asSymbol].
	^classPackageMapping removeKey: aClassName asSymbol ifAbsent: [self reportBogusBehaviorOf: #unregisterPackage:forClassName:  ]
]

{ #category : #registration }
RPackageOrganizer >> unregisterPackageNamed: symbol [
	"Unregister the specified package from the list of registered packages. Raise the announcement RPackageUnregistered."

	| aPackage |
	aPackage := packages at: symbol asSymbol ifAbsent: [ nil].
	aPackage
		ifNotNil: [self unregisterPackage: aPackage].
	^ aPackage
]

{ #category : #private }
RPackageOrganizer >> validateCanBeAddedPackage: aPackage [
	self validateCanBeAddedPackageName: aPackage name tagName: nil
]

{ #category : #private }
RPackageOrganizer >> validateCanBeAddedPackage: aPackage tagName: aSymbol [
	aPackage name = aSymbol
		ifTrue: [ ^ self ].
	self
		validateCanBeAddedPackageName: aPackage name , '-' , aSymbol
		tagName: nil
]

{ #category : #private }
RPackageOrganizer >> validateCanBeAddedPackageName: packageName tagName: tagName [
	| package |

	package := self
		packageNamed: packageName
		ifAbsent: [
			(packageName includes: $-) ifFalse: [ ^ self ].

			^ self
				validateCanBeAddedPackageName: (packageName copyUpToLast: $-)
				tagName: ((packageName copyAfterLast: $-), (tagName
					ifNotNil: [ '-', tagName ]
					ifNil: [ '' ] )) ].

	(tagName isEmptyOrNil or: [ package includesClassTagNamed: tagName ])
		ifTrue: [
			RPackageConflictError signal: ('Package/Tag can not be added because it conflicts with package {1} tag {2}' format: { packageName. tagName })  ]
]

{ #category : #registration }
RPackageOrganizer >> validatePackageDoesNotExist: aPackageName [

	(self includesPackageNamed: aPackageName)
		ifTrue: [ self signalPackageExists: aPackageName ]
]
