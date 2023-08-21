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

	| package protocol |
	"If the class is not packaged yet, ignore the situation. This method is created during the creation of the class or on an anonymous class"
	method methodClass package name = RPackage defaultPackageName ifTrue: [ ^ self ].

	protocol := method protocolName.
	package := (self hasPackageForProtocol: protocol)
		            ifTrue: [ self packageForProtocol: protocol inClass: method methodClass ]
		            ifFalse: [ self ensurePackage: (protocol copyWithout: $*) ].

	package addMethod: method
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

	Smalltalk allClassesAndTraits
		do: [ :behavior | (self ensurePackageMatching: behavior category) importClass: behavior ]
		displayingProgress: 'Importing behaviors'.

	Smalltalk allClassesAndTraits
		do: [ :behavior |
			{
				behavior.
				behavior classSide } do: [ :aBehavior |
				aBehavior extensionProtocols do: [ :protocol |
					(self ensurePackageMatching: protocol name allButFirst trimBoth) importProtocol: protocol forClass: aBehavior ] ] ]
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

	self validatePackageDoesNotExist: aString.

	^ self ensurePackage: aString
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

	^ self ensurePackage: RPackage defaultPackageName
]

{ #category : #registration }
RPackageOrganizer >> ensurePackage: aPackage [

	^ aPackage isString
		  ifTrue: [ self packageNamed: aPackage ifAbsent: [ self registerPackage: (RPackage named: aPackage organizer: self) ] ]
		  ifFalse: [
			  (self hasPackage: aPackage) ifFalse: [ self registerPackage: aPackage ].
			  aPackage ]
]

{ #category : #'system integration' }
RPackageOrganizer >> ensurePackageMatching: aString [
	"I try to find a package matching the string. The priority is given to a package having the same name case insensitive. If I don't find it, I'm stripping everything following the last dash and I retry to find the package. If non is found after all dashes are stripped, I create a package of the given name.
	
	This is useful when trying to find a package from a protocol name since we can add some dashes to protocol names after the package name. Or to find a package fron it's category until the categories are removed from the system."

	^ (self packageMatchingExtensionName: aString) ifNil: [ self ensurePackage: aString ]
]

{ #category : #registration }
RPackageOrganizer >> ensureTagNamed: aTagName inPackageNamed: aPackageName [

	^ (self ensurePackage: aPackageName) addClassTag: aTagName
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

	^ classExtendingPackagesMapping at: aClass instanceSide ifAbsent: [ #(  ) ]
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

{ #category : #testing }
RPackageOrganizer >> hasPackage: aPackage [
	"Takes a package or a package name as parameter and return true if I include this package."

	^ aPackage isString
		  ifTrue: [ packages includesKey: aPackage asSymbol ]
		  ifFalse: [ self packages includes: aPackage ]
]

{ #category : #'system integration' }
RPackageOrganizer >> hasPackageExactlyMatchingExtensionName: anExtensionName [

	| extension |
	extension := anExtensionName asSymbol.
	"fast version"
	(self hasPackage: extension) ifTrue: [ ^ true ].

	"slow version ignoring case"
	packages keysDo: [ :aSymbol | (aSymbol sameAs: extension) ifTrue: [ ^ true ] ].
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
RPackageOrganizer >> hasTag: aTag inPackage: aPackage [

	(self hasPackage: aPackage) ifFalse: [ ^ false ].

	^ (self ensurePackage: aPackage) hasTag: aTag
]

{ #category : #'deprecated - SystemOrganizer leftovers' }
RPackageOrganizer >> includesCategory: aString [
	"Tests if a category is already included."

	^ self categories
		  ifNil: [ false ]
		  ifNotNil: [ :categories | categories includes: aString ]
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

{ #category : #'system integration' }
RPackageOrganizer >> packageForProtocol: aProtocolName inClass: aClass [
	"According aProtocolName is an extension protocol, will look for a matching package. If no matching package is found ,  return nil. If aProtocolName is not an extension protocol, return the parent package of aClass"

	aProtocolName ifNil: [ ^ aClass package ].

	^ aProtocolName first = $*
		  ifTrue: [ self packageMatchingExtensionName: aProtocolName allButFirst ]
		  ifFalse: [ aClass package ]
]

{ #category : #'system integration' }
RPackageOrganizer >> packageMatchingExtensionName: anExtensionName [
	"I try to find a package matching the string. The priority is given to a package having the same name case insensitive. If I don't find it, I'm stripping everything following the last dash and I retry to find the package. If non is found after all dashes are stripped, I return nil.
	
	This is useful when trying to find a package from a protocol name since we can add some dashes to protocol names after the package name. Or to find a package fron it's category until the categories are removed from the system."

	| tmpPackageName |
	"we first look if their is a package matching exactly the name specified"
	(self packageNamedIgnoreCase: anExtensionName ifAbsent: [ nil ]) ifNotNil: [ :package | ^ package ].

	"if no package was found, we try to find one matching the begining of the name specified"
	tmpPackageName := ''.
	packages keysDo: [ :aSymbol |
		(anExtensionName beginsWith: aSymbol asString , '-' caseSensitive: false) ifTrue: [ "we keep the longest package name found"
			aSymbol size > tmpPackageName size ifTrue: [ tmpPackageName := aSymbol ] ] ].

	^ tmpPackageName = ''
		  ifTrue: [ nil ]
		  ifFalse: [ self packageNamed: tmpPackageName ]
]

{ #category : #accessing }
RPackageOrganizer >> packageNamed: aSymbol [
	^ self
		packageNamed: aSymbol
		ifAbsent: [ KeyNotFound signalFor: aSymbol ]
]

{ #category : #accessing }
RPackageOrganizer >> packageNamed: aSymbol ifAbsent: errorBlock [

	^ packages at: aSymbol asSymbol ifAbsent: errorBlock
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

	^ classPackageMapping at: aClass instanceSide ifAbsent: [ self packageNamed: RPackage defaultPackageName ]
]

{ #category : #'package - access from class' }
RPackageOrganizer >> packageOfClassNamed: aName [

	self flag: #pharoFixMe. "Should probably return _UnunpackagedPackage instead of nil"
	classPackageMapping keysAndValuesDo: [ :class :package | class originalName = aName ifTrue: [ ^ package ] ].

	^ nil
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

	(classExtendingPackagesMapping at: aClass instanceSide ifAbsentPut: [ IdentitySet new ]) add: aPackage
]

{ #category : #'system integration' }
RPackageOrganizer >> registerInterestToAnnouncer: anAnnouncer [
	"There should be only one"

	anAnnouncer unsubscribe: self.

	anAnnouncer weak
		when: CategoryAdded send: #systemCategoryAddedActionFrom: to: self;
		when: ClassAdded send: #systemClassAddedActionFrom: to: self;
		when: ClassRecategorized send: #systemClassRecategorizedActionFrom: to: self;
		when: ProtocolAnnouncement send: #systemClassReorganizedActionFrom: to: self;
		when: MethodAdded send: #systemMethodAddedActionFrom: to: self;
		when: MethodModified send: #systemMethodModifiedActionFrom: to: self;
		when: MethodRecategorized send: #systemMethodRecategorizedActionFrom: to: self
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

	self validateCanBeAddedPackageName: aPackage name tagName: nil.

	self basicRegisterPackage: aPackage.
	aPackage extendedClasses do: [ :extendedClass | self registerExtendingPackage: aPackage forClass: extendedClass ].
	aPackage definedClasses do: [ :definedClass | self registerPackage: aPackage forClass: definedClass ].

	SystemAnnouncer announce: (PackageAdded to: aPackage).

	^ aPackage
]

{ #category : #'private - registration' }
RPackageOrganizer >> registerPackage: aPackage forClass: aClass [

	(aPackage includesClass: aClass) ifFalse: [ self error: aPackage name , ' does not includes the class ' , aClass name ].
	^ classPackageMapping at: aClass instanceSide put: aPackage
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
RPackageOrganizer >> removeCategory: aCategory [
	"Remove the category named, cat. Create an error notificiation if the
	category has any elements in it."

	| category |
	category := aCategory asSymbol.

	categoryMap
		at: category
		ifPresent: [ :classes | classes ifNotEmpty: [ ^ self error: 'Cannot remove non-empty category ' , category , '. Present classes: ' , classes printString ] ]
		ifAbsent: [ ^ self ].

	categoryMap removeKey: category.

	"Now that we managed the SystemOrganizer, let's update the package organizer."
	(self packageMatchingExtensionName: category) ifNotNil: [ :package |
		package name = category
			ifTrue: [ self removePackage: package ]
			ifFalse: [ package classTagNamed: (category withoutPrefix: package name , '-') ifPresent: [ :tag | package removeClassTag: tag name ] ] ]
]

{ #category : #'system integration' }
RPackageOrganizer >> removeClass: class [
	"Remove the class, the class backpointer, the extensions and the extension backPointer from the receiver and the class involved with the class named: className. className is a class name and should not be a metaclass one. "

	class isClassSide ifTrue: [ self error: 'We should only be able to remove classes and not instances of metaclass.' ].
	(self packageOf: class) removeClass: class.
	(self extendingPackagesOf: class) do: [ :extendedPackage | extendedPackage removeAllMethodsFromClass: class ]
]

{ #category : #cleanup }
RPackageOrganizer >> removeEmptyPackagesAndTags [
	"Remove empty packages and tags."

	self packages do: [ :package | package removeEmptyTags ].
	(self packages select: [ :package | package isEmpty ]) do: [ :emptyPackage | self removePackage: emptyPackage ]
]

{ #category : #registration }
RPackageOrganizer >> removePackage: aPackage [

	| package |
	(self hasPackage: aPackage) ifFalse: [ ^ self ].

	package := aPackage isString
		           ifTrue: [ self packageNamed: aPackage ]
		           ifFalse: [ aPackage ].

	"In the system we have one default package and we do not want to remove this one."
	package isDefault ifTrue: [ ^ self ].

	self basicUnregisterPackageNamed: package name.
	package extendedClasses do: [ :extendedClass | self unregisterExtendingPackage: package forClass: extendedClass ].
	package definedClasses do: [ :definedClass | self unregisterPackage: package forClass: definedClass ].

	self flag: #package. "The next line should be removed with the system organizer later."
	self removeCategory: package name.
	SystemAnnouncer announce: (PackageRemoved to: package).

	^ package
]

{ #category : #'deprecated - SystemOrganizer leftovers' }
RPackageOrganizer >> removeSystemCategory: category [
	"remove all the classes and traits associated with the category"

	(self orderedTraitsIn: category) , (self superclassOrder: category) reverseDo: [ :each | each removeFromSystem ].

	self removeCategory: category
]

{ #category : #'deprecated - SystemOrganizer leftovers' }
RPackageOrganizer >> removeTag: aTag fromPackage: aPackage [

	(self hasPackage: aPackage) ifFalse: [ ^ self "no need to remove the tag if the package is not there." ].

	(self ensurePackage: aPackage) removeTag: aTag
]

{ #category : #'deprecated - SystemOrganizer leftovers' }
RPackageOrganizer >> renameCategory: oldCatString toBe: newCatString [
	"Rename a category. No action if new name already exists, or if old name does not exist."

	| oldName newName package |
	oldName := oldCatString asSymbol.
	newName := newCatString asSymbol.

	categoryMap at: newName ifPresent: [ "new name exists, so no action" ^ self ].

	categoryMap at: oldName ifPresent: [ :classes |
		categoryMap
			at: newName put: classes;
			removeKey: oldName ].

	"We do it in all cases because we have a few cases where a category was not created but a package was. It's bugs but since categories will vanish there is no reason to care too much."
	package := self ensurePackageMatching: oldName.
	package name = oldName ifTrue: [ self renamePackage: package to: newName ].

	package classTagNamed: oldName ifPresent: [ :tag | tag basicRenameTo: newName ]
]

{ #category : #'system integration' }
RPackageOrganizer >> renamePackage: aPackage to: newName [

	(self hasPackage: aPackage) ifFalse: [ ^ self ].

	(self ensurePackage: aPackage) renameTo: newName
]

{ #category : #'system integration' }
RPackageOrganizer >> renameTag: aTag to: newName inPackage: aPackage [

	(self hasPackage: aPackage) ifFalse: [ ^ self ].

	(self ensurePackage: aPackage) renameTag: aTag to: newName
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

	(self ensurePackageMatching: ann categoryName) addClassTag: ann categoryName asSymbol
]

{ #category : #'system integration' }
RPackageOrganizer >> systemClassAddedActionFrom: ann [

	(self ensurePackageMatching: ann classAffected category) importClass: ann classAffected
]

{ #category : #'system integration' }
RPackageOrganizer >> systemClassRecategorizedActionFrom: announcement [

	| class newPackage newPackageName |
	class := announcement classAffected.

	newPackageName := announcement newCategory.

	newPackageName = announcement oldCategory ifTrue: [ ^ self ].
	newPackage := self ensurePackageMatching: newPackageName.

	newPackage moveClass: class toTag: (newPackage addClassTag: newPackageName)
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
	method origin = ann methodClass ifFalse: [ ^ self ].

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
	newMethod origin = ann methodClass ifFalse: [ ^ self ].


	"Special case for trait methods added an override"
	newMethod origin = oldMethod origin ifFalse: [ ^ self addMethod: newMethod ].

	methodPackage := newMethod package.
	"maybe the name of the method has changed"
	oldMethod selector = newMethod selector ifFalse: [
		methodPackage updateSelector: oldMethod selector inClass: oldMethod methodClass withNewSelector: newMethod selector ]
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
	(self packageOf: ann methodClass instanceSide) ifNil: [ ^ self ].

	newProtocol := ann newProtocol
		               ifNotNil: [ :protocol | protocol name ]
		               ifNil: [ ^ self ].

	oldProtocol := ann oldProtocol
		               ifNotNil: [ :protocol | protocol name ]
		               ifNil: [ '' ].

	methodPackage := method package.
	(newProtocol sameAs: oldProtocol) ifTrue: [ ^ self ].

	destinationPackage := (self hasPackageForProtocol: newProtocol)
		                      ifTrue: [ self packageForProtocol: newProtocol inClass: method methodClass ]
		                      ifFalse: [
			                      (newProtocol beginsWith: '*')
				                      ifTrue: [ self ensurePackage: newProtocol allButFirst capitalized ]
				                      ifFalse: [ method methodClass package ] ].

	methodPackage := (self hasPackageForProtocol: oldProtocol)
		                 ifTrue: [ self packageForProtocol: oldProtocol inClass: method methodClass ]
		                 ifFalse: [ method methodClass package ].

	methodPackage = destinationPackage ifTrue: [ ^ self ].

	(methodPackage methods includes: method) ifTrue: [ methodPackage removeMethod: method ].

	destinationPackage addMethod: method.

	SystemAnnouncer uniqueInstance methodRepackaged: method from: methodPackage to: destinationPackage
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

	classExtendingPackagesMapping at: aClass instanceSide ifPresent: [ :extendingPackages | extendingPackages remove: aPackage ifAbsent: [  ] ]
]

{ #category : #'system integration' }
RPackageOrganizer >> unregisterInterestToSystemAnnouncement [
	"self unregisterInterestToSystemAnnouncement"

	SystemAnnouncer uniqueInstance unsubscribe: self
]

{ #category : #'private - registration' }
RPackageOrganizer >> unregisterPackage: aPackage forClass: aClass [
	"unregister the back pointer mapping from classes to packages."

	self flag: #package. "When RPackage will be simplified we should not have to check the package but it is required for now"
	^ (classPackageMapping at: aClass instanceSide ifAbsent: [ ^ self ]) = aPackage ifTrue: [ classPackageMapping removeKey: aClass instanceSide ]
]

{ #category : #private }
RPackageOrganizer >> validateCanBeAddedPackageName: packageName tagName: tagName [

	| package |
	package := self packageNamed: packageName ifAbsent: [
		           (packageName includes: $-) ifFalse: [ ^ self ].

		           ^ self validateCanBeAddedPackageName: (packageName copyUpToLast: $-) tagName: (packageName copyAfterLast: $-) , (tagName
				              ifNotNil: [ '-' , tagName ]
				              ifNil: [ '' ]) ].

	(tagName isEmptyOrNil or: [ package includesClassTagNamed: tagName ]) ifTrue: [
		RPackageConflictError signal: ('Package/Tag can not be added because it conflicts with package {1} tag {2}' format: {
					 packageName.
					 tagName }) ]
]

{ #category : #registration }
RPackageOrganizer >> validatePackageDoesNotExist: aPackageName [

	(self hasPackage: aPackageName) ifTrue: [ self signalPackageExists: aPackageName ]
]
