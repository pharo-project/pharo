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
    => I don't see anything to do for this annoucement

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
		'debuggingName'
	],
	#classInstVars : [
		'default'
	],
	#category : #'RPackage-Core-Base'
}

{ #category : #singleton }
RPackageOrganizer class >> default [
	"WARNING: Since this component can be changed (i.e. for testing) you should NOT use it directly. 
	  Use RPackage class>>#organizer instead"
	
	^ default ifNil: [  default := self new ]
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
	(methodCategory isEmptyOrNil or:[ methodCategory first ~= $* ]) 
		ifFalse: [
			(self isPackageDefinedForClass: anEvent methodClass)
					ifFalse: [self packageClass new named: (Smalltalk organization categoryOfElement: anEvent methodClass instanceSide asSymbol) ].
		].
]

{ #category : #private }
RPackageOrganizer class >> packageClass [

	^ RPackage
]

{ #category : #'class initialization' }
RPackageOrganizer class >> registerInterestToSystemAnnouncement [

	self default unregisterInterestToSystemAnnouncement.
	"To make sure that we do not have it twice registered"
	self default registerInterestToSystemAnnouncement
]

{ #category : #singleton }
RPackageOrganizer class >> resetDefault [

	"self resetDefault"
	default  ifNotNil: [
		"When an Organizer is not used anymore, we should always pay attention that these two actions are corretly done."
		default unregisterInterestToSystemAnnouncement.
		
		self flag: #hack. "for decoupling MC"
		self class environment at: #MCWorkingCopy ifPresent: [ :wc |
			wc removeDependent: default]].
	default := self new.
	self packageClass initialize
]

{ #category : #private }
RPackageOrganizer class >> setDefault: anOrganizer [ 
	default ifNotNil: [ default unregister ].
	default := anOrganizer.
	default register.
	RPackage organizer: nil.
]

{ #category : #'class initialization' }
RPackageOrganizer class >> unregisterInterestToSystemAnnouncement [

	self default unregisterInterestToSystemAnnouncement
]

{ #category : #private }
RPackageOrganizer >> addMethod: method [
	"we have to register the method in the parent RPackage of the class. 
	to do that, we first have to look if the method is an extension from an external package:"

	| rPackage protocol |
	
	protocol := method protocol ifNil: [ Error signal: 'Should not happen' ].
	
	"If the class is not packaged yet, ignore the situation. This method is created during the creation of the class or on an anonymous class"
	(method methodClass package name = RPackage defaultPackageName) ifTrue: [ ^ self ].
	
	rPackage := (self hasPackageForProtocol: protocol inClass: method methodClass)
		ifTrue: [ self packageForProtocol: protocol inClass: method methodClass ]
		ifFalse: [ self registerPackage: (self packageClass named: (protocol copyWithout: $*)) ].
		
	rPackage addMethod: method
]

{ #category : #'system integration' }
RPackageOrganizer >> announcer [ 
	^SystemAnnouncer uniqueInstance private
]

{ #category : #'initialization-data' }
RPackageOrganizer >> basicInitializeFromPackagesList: aPackagesList [

	aPackagesList
		do: [ :packageName | 
		  | package |
		  package := (self packageClass named: packageName).
		  packages at: packageName asSymbol put: package. ]
		displayingProgress: 'Importing monticello packages'.
		
	Smalltalk allClassesAndTraits 
		do: [ :eachBehavior | self initializeFor: eachBehavior ]
		displayingProgress: 'Importing behaviors'.

	Smalltalk allClassesAndTraits 
		do: [ :eachBehavior | 
			self initializeMethodsFor: eachBehavior.
			self initializeMethodsFor: eachBehavior classSide ]
		displayingProgress: 'Importing methods'.

	Smalltalk allClassesAndTraits 
		do: [ :eachBehavior |
			eachBehavior organization extensionProtocols do: [ :eachProtocol | 
				self 
					initializeExtensionsFor: eachBehavior 
					protocol: eachProtocol ].
			eachBehavior classSide organization extensionProtocols do: [ :eachProtocol | 
				self 
					initializeExtensionsFor: eachBehavior classSide 
					protocol: eachProtocol ] ]
		displayingProgress: 'Importing extensions'.
]

{ #category : #'private registration' }
RPackageOrganizer >> basicRegisterPackage: aPackage [
	"A new package is now available and declared in the receiver. Note that it is a low level implementation method since it does not deal with package contained information and does not update the related mapping tables."
	
	packages 
		at: aPackage name asSymbol 
		put: aPackage.

	"register mc package"
	self flag: #hack. "for decoupling MC"
	self class environment at: #MCWorkingCopy ifPresent: [ :wc |
		wc forPackage: ((self class environment at: #MCPackage) named: aPackage name)].
	
	^ aPackage
]

{ #category : #'private registration' }
RPackageOrganizer >> basicUnregisterPackage: aPackage [
	"Unregister the specified package from the list of registered packages. Raise the announcement RPackageUnregistered. This is a low level action. It does not unregister the back pointer from classes to packages or any other information managed by the organizer"
	
	packages
		removeKey: aPackage name asSymbol
		ifAbsent: [ self reportExtraRemovalOf: aPackage ].

	"unregister also mc package"
	self flag: #hack. "for decoupling MC"
	self class environment at: #MCWorkingCopy ifPresent: [ 
		aPackage mcPackage 
			ifNotNil: [ :mcPackage | mcPackage workingCopy unregister ]].
]

{ #category : #'private registration' }
RPackageOrganizer >> basicUnregisterPackageNamed: aSymbolName [
	"Unregister the specified package from the list of registered packages. Raise the RPackageUnregistered announcement. This is a low level action. It does not unregister the back pointer from classes to packages or any other information managed by the organizer"
	
	packages
		removeKey: aSymbolName asSymbol
		ifAbsent: [ ]
	
]

{ #category : #'system compatibility' }
RPackageOrganizer >> category: categoryName matches: prefix [
	| prefixSize catSize |
	
	categoryName ifNil: [ ^false ].
	catSize := categoryName size.
	prefixSize := prefix size.
	catSize < prefixSize ifTrue: [ ^false ].
	(categoryName findString: prefix startingAt: 1 caseSensitive: false) = 1
		ifFalse: [ ^false ].
	^(categoryName at: prefix size + 1 ifAbsent: [ ^true ]) = $-
]

{ #category : #'private registration' }
RPackageOrganizer >> checkPackageExistsOrRegister: packageName [
	(self packages 
		anySatisfy: [ :each | self category: packageName matches: each packageName ])
		ifFalse: [ (self packageClass named: packageName capitalized) register ] 
]

{ #category : #'public registration' }
RPackageOrganizer >> createPackageNamed: aString [ 

	| instance |
	self validatePackageDoesNotExist: aString.
	
	instance := self packageClass named: aString.
	self registerPackage: instance.
	^ instance
]

{ #category : #'private testing' }
RPackageOrganizer >> debuggingName [

	^ debuggingName 
]

{ #category : #'private testing' }
RPackageOrganizer >> debuggingName: aString [

	debuggingName := aString
]

{ #category : #initialization }
RPackageOrganizer >> defineUnpackagedClassesPackage [
	^ self ensureExistAndRegisterPackageNamed: self packageClass defaultPackageName
]

{ #category : #'private registration' }
RPackageOrganizer >> ensureExistAndRegisterPackageNamed: aSymbol [
	"A new package is now available and declared in the receiver."
	
	| package |
	
	package := packages 
		at: aSymbol asSymbol 
		ifAbsentPut: [ self packageClass named: aSymbol ].
		
	package extendedClasses do: [ :extendedClass|
			self registerExtendingPackage: package forClass: extendedClass].
	package definedClasses do: [ :definedClass|
			self registerPackage: package forClass: definedClass].
	
	SystemAnnouncer uniqueInstance announce: (RPackageRegistered to: package).
	
	^ package
]

{ #category : #'package access from class' }
RPackageOrganizer >> extendingPackagesOf: aClass [
	"Returns the packages extending the class aClass"
	^ classExtendingPackagesMapping at: aClass instanceSide name ifAbsent: [#()]
]

{ #category : #'package access from class' }
RPackageOrganizer >> extendingPackagesOfClassNamed: aName [
	"Returns the packages extending the class named a Symbol"
	^ classExtendingPackagesMapping at: aName asSymbol ifAbsent: [#()]
]

{ #category : #'system integration' }
RPackageOrganizer >> fullyRemoveClassNamed: className [
	"Remove the class, the class backpointer, the extensions and the extension backPointer from the receiver and the class involved with the class named: className. className is a class name and should not be a metaclass one. "	
	| rPackage |	
		
	rPackage := self packageOfClassNamed: className.
	rPackage ifNil: [ ^ self ].
	rPackage removeClassNamed: className.
	(self extendingPackagesOfClassNamed: className)
		do: [:each | each removeAllMethodsFromClassNamed: className. ].

	
	
]

{ #category : #'package access from class' }
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
RPackageOrganizer >> hasPackageForProtocol: aProtocolName inClass: aClass [
	"According aProtocolName is an extension protocol, will look for a matching package. If no matching package is found ,  return nil. If aProtocolName is not an extension protocol, return the parent package of aClass"
	
	|tmpProtocol|
	tmpProtocol := aProtocolName ifNil: [''].
	^ (tmpProtocol beginsWith: '*') not or: [ self hasPackageMatchingExtensionName:  (tmpProtocol copyWithout: $*) ]
]

{ #category : #'system integration' }
RPackageOrganizer >> hasPackageMatchingExtensionName: anExtensionName [

	(self hasPackageExactlyMatchingExtensionName: anExtensionName)
		ifTrue: [ ^true ].
		
	packages keysDo: [ :aSymbol | 
		(anExtensionName beginsWithEmpty: aSymbol, '-' caseSensitive: false)
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

{ #category : #testing }
RPackageOrganizer >> includesPackage: aPackage [
	"Answer whether the receiver get a package as registered."
	^ self includesPackageNamed: aPackage name
]

{ #category : #'private testing' }
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
	
	self defineUnpackagedClassesPackage
]

{ #category : #'initialization-data' }
RPackageOrganizer >> initializeExtensionsFor: aBehavior protocol: aProtocol [
	| package protocolName nonTraitMethods |
	
	protocolName := (aProtocol name allButFirst) trimBoth.
	package := self packageMatchingExtensionName: protocolName.
	package ifNil: [
		package := self basicRegisterPackage: (self packageClass named: protocolName) ].
	nonTraitMethods := aProtocol methodSelectors 
		select: [ :eachSelector | (aBehavior >> eachSelector) origin = aBehavior ].
	nonTraitMethods ifEmpty:[^ self].
	self registerExtendingPackage: package forClass: aBehavior.
	nonTraitMethods
		do: [ :eachSelector | package addMethod: aBehavior >> eachSelector ]
]

{ #category : #'initialization-data' }
RPackageOrganizer >> initializeFor: aBehavior [
	| package |
		
	package := self packageMatchingExtensionName: aBehavior category.
	package ifNil: [ 
		"It should not happen. 
		 But actually could happen that one class is in a SystemCategory and not in a MC"
		package := self basicRegisterPackage: (self packageClass named: aBehavior category) ]. 
	package addClassDefinition: aBehavior.
	package 
		addClassDefinition: aBehavior 
		toClassTag: aBehavior category asSymbol.
	self registerPackage: package forClass: aBehavior.
]

{ #category : #'initialization-data' }
RPackageOrganizer >> initializeFromPackagesList: aPackagesList [
 	self packageClass 
		withOrganizer: self  
		do:  [  self basicInitializeFromPackagesList: aPackagesList ].
]

{ #category : #'initialization-data' }
RPackageOrganizer >> initializeMethodsFor: aBehavior [
	| package |
	
	package := aBehavior package.  
	(aBehavior organization protocols
		select: [ :each | each isExtensionProtocol not ])
		do: [ :eachProtocol | 
			 (eachProtocol methodSelectors 
				select: [ :eachSelector | (aBehavior >> eachSelector) origin = aBehavior ])
				do: [ :eachSelector | package addMethod: (aBehavior >> eachSelector) ] ]
]

{ #category : #accessing }
RPackageOrganizer >> packageClass [
	"Return the system class that represent packages."
	
	^ RPackage
	
]

{ #category : #accessing }
RPackageOrganizer >> packageDefiningOrExtendingMethod: aCompiledMethod [
	^ self packageDefiningOrExtendingSelector: aCompiledMethod selector inClass: aCompiledMethod methodClass

]

{ #category : #accessing }
RPackageOrganizer >> packageDefiningOrExtendingSelector: aSelector inClass: aClass [
	"this implementation is slower
		aClass packages detect: [:each | each includesSelector: aSelector ofClass: aClass ]"

	
	^ (aClass packages) detect: [:each | each includesSelector: aSelector ofClass: aClass ] ifNone: [
		"if we do not find in the packages of the class, it means that the method is coming from a trait: "
		|tmpTrait|
		tmpTrait := (aClass traitComposition traitProvidingSelector: aSelector).
		(aClass traitComposition traitProvidingSelector: aSelector) packages detect: [:each |
			each includesSelector: aSelector ofClass: tmpTrait.   
			].
		]
	
"	self packages detect: [:each | 
		each includesSelector: aSelector ofClass: aClass ].
"	
	
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
RPackageOrganizer >> packageExactlyMatchingExtensionName: anExtensionName [
	"only look for a package for which the name match 'anExtensionName', making no difference about case. Return nil if no package is found"
	
	| extension |
	extension := anExtensionName asSymbol.
	
	packages keysDo: [:aSymbol |
		(aSymbol sameAs: extension)
			ifTrue: [ ^ self packageNamed: aSymbol ]].

	^ nil
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
	(self packageExactlyMatchingExtensionName: anExtensionName)
		ifNotNil: [ :package | ^ package ].
	
	"if no package was found, we try to find one matching the begining of the name specified"
	tmpPackageName := ''.
	packages keysDo: [:aSymbol |
		(anExtensionName beginsWithEmpty: (aSymbol, '-') caseSensitive: false)
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
		
	^ aBlock value.
]

{ #category : #accessing }
RPackageOrganizer >> packageNames [
	^ packages keys
]

{ #category : #accessing }
RPackageOrganizer >> packageNamesDo: aBlock [
	^ packages keysDo: aBlock
]

{ #category : #'package access from class' }
RPackageOrganizer >> packageOf: aClass [

	^ classPackageMapping 
		at: aClass instanceSide originalName
		ifAbsent: [self packageNamed: self packageClass defaultPackageName]
]

{ #category : #'package access from class' }
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
	debuggingName ifNotNil: [aStream nextPutAll: debuggingName ].  
]

{ #category : #initialization }
RPackageOrganizer >> register [

	"self registerInterestToMC."
	self registerInterestToSystemAnnouncement.
]

{ #category : #'private registration' }
RPackageOrganizer >> registerExtendingPackage: aPackage forClass: aClass [

	| cur |

	cur := classExtendingPackagesMapping at: aClass instanceSide name ifAbsent: [ nil ].
	cur
		ifNil: [ classExtendingPackagesMapping at: aClass instanceSide name put: ( Set with: aPackage ) ]
		ifNotNil: [ cur add: aPackage ]
]

{ #category : #'private registration' }
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
		when:  ClassAdded send: #systemClassAddedActionFrom: to: self;
		when:  ClassRecategorized send: #systemClassRecategorizedActionFrom: to: self;
		when:  ClassRemoved send: #systemClassRemovedActionFrom: to: self;
		when:  ClassRenamed send: #systemClassRenamedActionFrom: to: self;
		when:  ClassReorganized send: #systemClassReorganizedActionFrom: to: self;
		when:  MethodAdded send: #systemMethodAddedActionFrom: to: self;
		when:  MethodModified send: #systemMethodModifiedActionFrom: to: self;
		when:  MethodRecategorized send: #systemMethodRecategorizedActionFrom: to: self;
		when:  MethodRemoved send: #systemMethodRemovedActionFrom: to: self;
		when:  ProtocolAdded send: #systemProtocolAddedActionFrom: to: self;
		when:  ProtocolRemoved send: #systemProtocolRemovedActionFrom: to: self.
		
	self flag: #hack. "for decoupling MC"
	self class environment at: #MCWorkingCopy ifPresent: [ 
		anAnnouncer weak
			when: (self class environment at: #MCWorkingCopyCreated) 
				send: #updateAfterNewMCPackageRegistred: 
				to: self;
			when: (self class environment at: #MCWorkingCopyDeleted) 
				send: #updateAfterNewMCPackageUnregistred: 
				to: self	].
]

{ #category : #'system integration' }
RPackageOrganizer >> registerInterestToSystemAnnouncement [
	"self registerInterestToSystemAnnouncement"
	<systemEventRegistration>
	
	self registerInterestToAnnouncer: self announcer
]

{ #category : #'public registration' }
RPackageOrganizer >> registerPackage: aPackage [
	"A new package is now available and declared in the receiver. "
	
	self validateCanBeAddedPackage: aPackage.
	
	self basicRegisterPackage: aPackage.
	aPackage extendedClasses 
		do: [ :extendedClass | self registerExtendingPackage: aPackage forClass: extendedClass].
	aPackage definedClasses 
		do: [ :definedClass | self registerPackage: aPackage forClass: definedClass].
		
	SystemAnnouncer uniqueInstance announce: (RPackageRegistered to: aPackage).
	
	^ aPackage
]

{ #category : #'private registration' }
RPackageOrganizer >> registerPackage: aPackage forClass: aClass [ 
	(aPackage includesClass: aClass) 
		ifFalse: 
			[self error: aPackage name , ' does not includes the class ' , aClass name].
	^classPackageMapping at: aClass instanceSide name put: aPackage
]

{ #category : #'private registration' }
RPackageOrganizer >> registerPackage: aPackage forClassName: aClassNameSymbol [
	"Register aPackage as the package of the class aClassNameSymbol. The package should contain the class in its class definitions."
	
	(aPackage includesClassNamed: aClassNameSymbol asSymbol) 
		ifFalse: 
			[self error: aPackage name , ' does not include the class ' , aClassNameSymbol].
	^classPackageMapping at: aClassNameSymbol put: aPackage
]

{ #category : #'public registration' }
RPackageOrganizer >> registerPackageNamed: aString [ 
	^ self 
		packageNamed: aString asSymbol
		ifAbsent: [ (self packageClass named: aString asSymbol) register ]
]

{ #category : #'system integration' }
RPackageOrganizer >> renamePackage: rPackage from: oldName to: newName [ 
	| classesAndProtocolsToRename |
	
	rPackage name: newName. 
	
	"we update the organizer"
	packages 
		at: newName 
		put: rPackage.
	packages 
		removeKey: oldName 
		ifAbsent: [ self reportBogusBehaviorOf:   #systemCategoryRenamedActionFrom: ]. 
	
	"we also rename all the extension protocols in the system with the new name"
	classesAndProtocolsToRename := rPackage extensionMethods asIdentitySet.
	classesAndProtocolsToRename 
		do: [:method | 
			method methodClass organization 
				silentlyRenameCategory: method category 
				toBe: '*', newName ]
]

{ #category : #private }
RPackageOrganizer >> reportBogusBehaviorOf: aSelector [

	self traceCr: 'RPackage log: Something wrong around ', aSelector asString , 'since the removeKey: is called on not present information.'.
]

{ #category : #private }
RPackageOrganizer >> reportExtraRemovalOf: aClass [

	self traceCr: 'The class ', aClass name printString , ' is removed twice'
]

{ #category : #'public registration' }
RPackageOrganizer >> signalPackageExists: aPackageName [

	RPackageConflictError signal: ('A package named {1} already exists' format: {aPackageName})
]

{ #category : #private }
RPackageOrganizer >> stopNotification [
	"(self instVarNamed: #default) stopNotification"
	
	"pay attention that we can break the system using this method"
	
	SystemAnnouncer uniqueInstance unsubscribe: self.
	
	self class environment at: #MCWorkingCopy ifPresent: [:wc | 
		wc removeDependent: self].
]

{ #category : #'system integration' }
RPackageOrganizer >> systemCategoryAddedActionFrom: ann [
	| package |
	
	package := self packageMatchingExtensionName: ann categoryName asString.
	package ifNil: [ 
		package := self registerPackage: (self packageClass named: ann categoryName asSymbol) ].
	package addClassTag: ann categoryName asSymbol.
]

{ #category : #'system integration' }
RPackageOrganizer >> systemCategoryRemovedActionFrom: ann [
	"When a system category is removed, we may: remove a tag, or remove a rpackage. If we remove a RPackage, unregister the linked MCWorkingCopy. If it is a tag, do nothing? (from what I know of RPackage, the tag should already have disappeared because it would have been empty)."

	| rPackage categoryName categoryNameSymbol managers |
	categoryName := ann categoryName.
	categoryNameSymbol := categoryName asSymbol.
	self flag: #hack.	"for decoupling MC"
	managers := (self respondsTo: #allManagers)
		ifTrue: [ self perform: #allManagers ]
		ifFalse: [ #() ].	
	"Reverse the test. First check that this is the root category of a RPackage. If yes, unregister the RPackage and the corresponding MCWorkingCopy."
	rPackage := packages at: categoryNameSymbol ifAbsent: [ ^ self ].
	"Consider that a rPackage with extension selectors or tags is not empty and shouldn't be removed."
	(rPackage extensionSelectors notEmpty
		or: [ (rPackage classTags reject: [ :tag | tag name = categoryName ]) notEmpty ])
		ifTrue: [ ^ self ].
	self basicUnregisterPackageNamed: categoryNameSymbol.
	(managers detect: [ :each | each packageName = categoryName ] ifNone: [  ])
		ifNotNil: [ :mcWorkingCopy | mcWorkingCopy unregister ]
]

{ #category : #'system integration' }
RPackageOrganizer >> systemCategoryRenamedActionFrom: ann [
	| rPackage oldName newName |

	oldName := ann oldCategoryName asSymbol.
	newName := ann newCategoryName asSymbol.
	rPackage := self packageMatchingExtensionName: ann oldCategoryName.
	rPackage ifNil: [ rPackage := (self packageClass named: newName) register ].
	rPackage name = ann oldCategoryName ifTrue: [ 
		self 
			renamePackage: rPackage 
			from: oldName 
			to: newName ].
		
	rPackage 
		classTagNamed: oldName
		ifPresent: [ :tag | tag basicRenameTo: newName ]

]

{ #category : #'system integration' }
RPackageOrganizer >> systemClassAddedActionFrom: ann [
	| class rPackage categoryNameSymbol |
	
	class := ann classAffected.

	categoryNameSymbol := class category.
	rPackage := (self packageMatchingExtensionName: categoryNameSymbol) 
		ifNil: [ self registerPackage: (self packageClass named: categoryNameSymbol) ].		
	
	rPackage importClass: class.

]

{ #category : #'system integration' }
RPackageOrganizer >> systemClassRecategorizedActionFrom: ann [
	| class newRPackage newRPackageTag oldRPackage newCategoryName oldCategoryName|
	
	class := ann classAffected.	

	newCategoryName := ann newCategory asSymbol.
	oldCategoryName := ann oldCategory asSymbol.
	oldRPackage := self packageMatchingExtensionName: oldCategoryName includingClass: class.
	newRPackage := (self packageMatchingExtensionName: newCategoryName) 
		ifNil: [ self registerPackage: (self packageClass named: newCategoryName) ].

	newRPackageTag := newRPackage addClassTag: newCategoryName.

	newRPackage 
		moveClass: class 
		fromPackage: oldRPackage 
		toTag: newRPackageTag
]

{ #category : #'system integration' }
RPackageOrganizer >> systemClassRemovedActionFrom: ann [
	"A class has been removed, we should update the package adequately."
	
	| class className |	
	class := ann classRemoved.
	className  := class originalName.
		
	self fullyRemoveClassNamed: className.
	
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
	| clazz classOrganization |

	clazz := ann classReorganized.
	classOrganization := clazz organization.
	classOrganization categories do: [ :each | 
		(each beginsWith: '*')
			ifTrue:  [ self checkPackageExistsOrRegister: each allButFirst ] ]

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
		ifFalse: [ methodPackage updateSelector: oldMethod selector inClass: oldMethod methodClass withNewSelector: newMethod selector].
		

	
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

	method := ann method.
	method ifNil: [ ^ self ].
	method origin = ann methodClass ifFalse: [ ^ self].
	(self packageOfClassNamed: ann methodClass instanceSide name) ifNil: [ ^ self ].

	newProtocol := ann  newProtocol.
	newProtocol ifNil: [ ^ self ].

	oldProtocol := ann oldProtocol ifNil: [ '' ].
	
	methodPackage := method packageFromOrganizer: self.
	newProtocol asLowercase = oldProtocol asLowercase ifTrue: [ ^ self ].
		
	destinationPackage := (self hasPackageForProtocol: newProtocol inClass: method methodClass)
		ifTrue: [ 
			self packageForProtocol: newProtocol inClass: method methodClass ]
		ifFalse: [ 
			(newProtocol beginsWith: '*')
				ifTrue: [ self ensureExistAndRegisterPackageNamed: newProtocol allButFirst capitalized ]
				ifFalse: [ method methodClass package ] ].
			
	methodPackage := (self hasPackageForProtocol: oldProtocol inClass: method methodClass)
		ifTrue: [ self packageForProtocol: oldProtocol inClass: method methodClass ]
		ifFalse: [ method methodClass package ].

	methodPackage = destinationPackage
		ifFalse: [
			(methodPackage methods includes: method) 
				ifTrue: [ methodPackage removeMethod: method ].
			
			destinationPackage addMethod: method.
			
			SystemAnnouncer uniqueInstance
				methodRepackaged: method
				from: methodPackage
				to: destinationPackage ]
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

{ #category : #'system integration' }
RPackageOrganizer >> systemProtocolAddedActionFrom: ann [

	"for now I don't see anything to do'"
]

{ #category : #'system integration' }
RPackageOrganizer >> systemProtocolRemovedActionFrom: ann [
	"to be unregistered"
	
	
]

{ #category : #initialization }
RPackageOrganizer >> unregister [
	SystemAnnouncer uniqueInstance unsubscribe: self.
]

{ #category : #'private registration' }
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

{ #category : #'private registration' }
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

{ #category : #'public registration' }
RPackageOrganizer >> unregisterPackage: aPackage [
	"Unregister the specified package from the list of registered packages. Raise the announcement RPackageUnregistered."

	self basicUnregisterPackage: aPackage.
	aPackage extendedClasses 
		do: [ :extendedClass | self unregisterExtendingPackage: aPackage forClass: extendedClass].
	aPackage definedClasses 
		do: [ :definedClass | self unregisterPackage: aPackage forClass: definedClass].
	SystemAnnouncer uniqueInstance announce: (RPackageUnregistered to: aPackage).
	
	^ aPackage
]

{ #category : #'private registration' }
RPackageOrganizer >> unregisterPackage: aPackage forClass: aClass [ 
	"unregister the back pointer mapping from classes to packages."
		
	^classPackageMapping 
		removeKey: aClass instanceSide name 
		ifAbsent: [ self reportExtraRemovalOf: aClass ]
]

{ #category : #'private registration' }
RPackageOrganizer >> unregisterPackage: aPackage forClassName: aClassName [
	"Unregister the package back pointer for a given class. The class should not belong to the package anymore before removing the back pointer."

	(aPackage includesClassNamed: aClassName asSymbol) 
		ifTrue: 
			[self error: aPackage name , ' still includes the class ' , aClassName asSymbol].
	^classPackageMapping removeKey: aClassName asSymbol ifAbsent: [self reportBogusBehaviorOf: #unregisterPackage:forClassName:  ]
]

{ #category : #'public registration' }
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

{ #category : #'public registration' }
RPackageOrganizer >> validatePackageDoesNotExist: aPackageName [

	(self includesPackageNamed: aPackageName)
		ifTrue: [ self signalPackageExists: aPackageName ]
]
