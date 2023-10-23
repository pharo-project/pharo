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
	#name : 'RPackageOrganizer',
	#superclass : 'Object',
	#instVars : [
		'classPackageMapping',
		'packages',
		'classExtendingPackagesMapping',
		'environment'
	],
	#category : 'RPackage-Core-Base',
	#package : 'RPackage-Core',
	#tag : 'Base'
}

{ #category : 'singleton' }
RPackageOrganizer class >> default [
	"Warning: Use 'self packageOrganizer' since I will be removed in the future."

	^ self packageOrganizer
]

{ #category : 'class initialization' }
RPackageOrganizer class >> initialize [
	"You need manually filll the data:

	self initializeDefaultFromMC.

	is the usual way (but is not in this package by default, it depends on MC)"
]

{ #category : 'quieries' }
RPackageOrganizer class >> isPackageDefinedForClass: aClass [

	^ (self default packageOf: aClass) notNil
]

{ #category : 'class initialization' }
RPackageOrganizer class >> registerInterestToSystemAnnouncement [

	<script>
	self packageOrganizer unregisterInterestToSystemAnnouncement.
	"To make sure that we do not have it twice registered"
	self packageOrganizer registerInterestToSystemAnnouncement
]

{ #category : 'class initialization' }
RPackageOrganizer class >> unregisterInterestToSystemAnnouncement [

	self default unregisterInterestToSystemAnnouncement
]

{ #category : 'private' }
RPackageOrganizer >> addMethod: method [
	"we have to register the method in the parent RPackage of the class.
	to do that, we first have to look if the method is an extension from an external package:"

	| package protocol |
	"If the class is not packaged yet, ignore the situation. This method is created during the creation of the class or on an anonymous class"
	method methodClass package name = UndefinedPackage undefinedPackageName ifTrue: [ ^ self ].

	protocol := method protocol.
	package := protocol isExtensionProtocol
		           ifTrue: [ self ensurePackageOfExtensionProtocol: protocol ]
		           ifFalse: [ method methodClass package ].

	package addMethod: method
]

{ #category : 'registration' }
RPackageOrganizer >> addPackage: aPackage [

	| package |
	self validatePackageDoesNotExist: aPackage.

	aPackage isString
		ifTrue: [ package := RPackage named: aPackage organizer: self ]
		ifFalse: [
			aPackage organizer: self.
			aPackage extendedClasses do: [ :extendedClass | self registerExtendingPackage: aPackage forClass: extendedClass ].
			aPackage definedClasses do: [ :definedClass | self registerPackage: aPackage forClass: definedClass ].
			package := aPackage ].

	self basicRegisterPackage: package.

	SystemAnnouncer announce: (PackageAdded to: package).

	^ package
]

{ #category : 'system integration' }
RPackageOrganizer >> announcer [
	^SystemAnnouncer uniqueInstance private
]

{ #category : 'initialization - data' }
RPackageOrganizer >> basicInitializeFromPackagesList: aPackagesList [

	aPackagesList do: [ :packageName | self ensurePackage: packageName ] displayingProgress: 'Importing monticello packages'.

	Smalltalk allClassesAndTraits
		do: [ :behavior |
			| package |
			package := self ensurePackageMatching: behavior category.
			package moveClass: behavior toTag: (package toTagName: behavior category) ]
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

{ #category : 'private - registration' }
RPackageOrganizer >> basicRegisterPackage: aPackage [
	"A new package is now available and declared in the receiver. Note that it is a low level implementation method since it does not deal with package contained information and does not update the related mapping tables."

	^ packages at: aPackage name asSymbol put: aPackage
]

{ #category : 'private - registration' }
RPackageOrganizer >> basicUnregisterPackage: aPackage [
	"Unregister the specified package from the list of registered packages. Raise the PackageRemoved announcement. This is a low level action. It does not unregister the back pointer from classes to packages or any other information managed by the organizer"

	^ packages removeKey: aPackage name ifAbsent: [  ]
]

{ #category : 'registration' }
RPackageOrganizer >> ensurePackage: aPackage [

	^ self packageNamed: (aPackage isString ifTrue: [ aPackage ] ifFalse: [ aPackage name ]) ifAbsent: [ self addPackage: aPackage ]
]

{ #category : 'registration' }
RPackageOrganizer >> ensurePackage: aPackage withTags: aCollection [

	| package |
	package := self ensurePackage: aPackage.
	aCollection do: [ :tag | package ensureTag: tag ]
]

{ #category : 'system integration' }
RPackageOrganizer >> ensurePackageMatching: aString [
	"I try to find a package matching the string. The priority is given to a package having the same name case insensitive. If I don't find it, I'm stripping everything following the last dash and I retry to find the package. If non is found after all dashes are stripped, I create a package of the given name.
	
	This is useful when trying to find a package from a protocol name since we can add some dashes to protocol names after the package name. Or to find a package fron it's category until the categories are removed from the system."

	^ (self packageMatchingExtensionName: aString) ifNil: [ self ensurePackage: aString ]
]

{ #category : 'system integration' }
RPackageOrganizer >> ensurePackageOfExtensionProtocol: aProtocol [

	^ (self packageForProtocol: aProtocol) ifNil: [ self ensurePackage: aProtocol name allButFirst capitalized ]
]

{ #category : 'registration' }
RPackageOrganizer >> ensureTag: aTagName inPackage: aPackageName [

	^ (self ensurePackage: aPackageName) ensureTag: aTagName
]

{ #category : 'accessing' }
RPackageOrganizer >> environment [
	 ^ environment ifNil: [ environment := Smalltalk globals]
]

{ #category : 'accessing' }
RPackageOrganizer >> environment: aSystemDictionary [
	 environment := aSystemDictionary
]

{ #category : 'package - access from class' }
RPackageOrganizer >> extendingPackagesOf: aClass [
	"Returns the packages extending the class aClass"

	^ classExtendingPackagesMapping at: aClass instanceSide ifAbsent: [ #(  ) ]
]

{ #category : 'testing' }
RPackageOrganizer >> hasPackage: aPackage [
	"Takes a package or a package name as parameter and return true if I include this package."

	^ aPackage isString
		  ifTrue: [
			  self packageNamed: aPackage asSymbol ifAbsent: [ ^ false ].
			  ^ true ]
		  ifFalse: [ self packages includes: aPackage ]
]

{ #category : 'testing' }
RPackageOrganizer >> hasTag: aTag inPackage: aPackage [

	(self hasPackage: aPackage) ifFalse: [ ^ false ].

	^ (self ensurePackage: aPackage) hasTag: aTag
]

{ #category : 'initialization' }
RPackageOrganizer >> initialize [

	super initialize.

	packages := IdentityDictionary new.
	classPackageMapping := IdentityDictionary new.
	classExtendingPackagesMapping := IdentityDictionary new.
	self ensurePackage: UndefinedPackage new
]

{ #category : 'system integration' }
RPackageOrganizer >> packageForProtocol: aProtocol [
	"Return nil if the protocol is not an extension of if there is no package matching the name.
	
	Else return the package matching the protocol. The priority is given to a package with the same name as the protocol or a package whose is a prefix of the protocol followed by a dash.
	I work in a case insensitive way."

	"If the protocol is nil or isnt an extension protocol then we have the package since it's the same as the class."
	(aProtocol isNil or: [ aProtocol isExtensionProtocol not ]) ifTrue: [ ^ nil ].

	^ self packageMatchingExtensionName: (aProtocol name copyWithout: $*)
]

{ #category : 'system integration' }
RPackageOrganizer >> packageMatchingExtensionName: anExtensionName [
	"I try to find a package matching the string. The priority is given to a package having the same name case insensitive. If I don't find it, I'm stripping everything following the last dash and I retry to find the package. If non is found after all dashes are stripped, I return nil.
	
	This is useful when trying to find a package from a protocol name since we can add some dashes to protocol names after the package name. Or to find a package fron it's category until the categories are removed from the system."

	| tmpPackageName |
	"we first look if their is a package matching exactly the name specified"
	self packageNamed: anExtensionName ifPresent: [ :package | ^ package ].

	"if no package was found, we try to find one matching the begining of the name specified"
	tmpPackageName := ''.
	self packageNames do: [ :aSymbol |
		(anExtensionName beginsWith: aSymbol asString , '-' caseSensitive: false) ifTrue: [ "we keep the longest package name found"
			aSymbol size > tmpPackageName size ifTrue: [ tmpPackageName := aSymbol ] ] ].

	^ tmpPackageName = ''
		  ifTrue: [ nil ]
		  ifFalse: [ self packageNamed: tmpPackageName ]
]

{ #category : 'accessing' }
RPackageOrganizer >> packageNamed: aSymbol [
	^ self
		packageNamed: aSymbol
		ifAbsent: [ KeyNotFound signalFor: aSymbol ]
]

{ #category : 'accessing' }
RPackageOrganizer >> packageNamed: aSymbol ifAbsent: errorBlock [
	"We first look at the fast solution then we try the case insensitive way because we do not care about the case in this package manager."

	^ packages at: aSymbol asSymbol ifAbsent: [
		  self packagesDo: [ :each | (each name sameAs: aSymbol) ifTrue: [ ^ each ] ].
		  errorBlock value ]
]

{ #category : 'accessing' }
RPackageOrganizer >> packageNamed: aSymbol ifPresent: aBlock [
	"We first look at the fast solution then we try the case insensitive way because we do not care about the case in this package manager."

	^ packages
		  at: aSymbol asSymbol
		  ifPresent: aBlock
		  ifAbsent: [
			  self packagesDo: [ :each | (each name sameAs: aSymbol) ifTrue: [ ^ aBlock cull: each ] ].
			  nil ]
]

{ #category : 'package - names-cache' }
RPackageOrganizer >> packageNames [

	^ packages keys
]

{ #category : 'package - names-cache' }
RPackageOrganizer >> packageNamesDo: aBlock [

	^ self packageNames do: aBlock
]

{ #category : 'package - access from class' }
RPackageOrganizer >> packageOf: aClass [

	^ classPackageMapping at: aClass instanceSide ifAbsent: [ self undefinedPackage ]
]

{ #category : 'package - access from class' }
RPackageOrganizer >> packageOfClassNamed: aName [

	classPackageMapping keysAndValuesDo: [ :class :package | class originalName = aName ifTrue: [ ^ package ] ].

	^ self undefinedPackage
]

{ #category : 'accessing' }
RPackageOrganizer >> packages [

	^ packages values
]

{ #category : 'accessing' }
RPackageOrganizer >> packagesDo: aBlock [

	self packages do: aBlock
]

{ #category : 'initialization' }
RPackageOrganizer >> register [

	"self registerInterestToMC."
	self registerInterestToSystemAnnouncement
]

{ #category : 'private - registration' }
RPackageOrganizer >> registerExtendingPackage: aPackage forClass: aClass [

	(classExtendingPackagesMapping at: aClass instanceSide ifAbsentPut: [ IdentitySet new ]) add: aPackage
]

{ #category : 'system integration' }
RPackageOrganizer >> registerInterestToAnnouncer: anAnnouncer [
	"There should be only one"

	anAnnouncer unsubscribe: self.

	anAnnouncer weak
		when: ClassAdded send: #systemClassAddedActionFrom: to: self;
		when: ClassRecategorized send: #systemClassRecategorizedActionFrom: to: self;
		when: MethodAdded send: #systemMethodAddedActionFrom: to: self;
		when: MethodRecategorized send: #systemMethodRecategorizedActionFrom: to: self
]

{ #category : 'system integration' }
RPackageOrganizer >> registerInterestToSystemAnnouncement [
	"self registerInterestToSystemAnnouncement"
	<systemEventRegistration>

	self registerInterestToAnnouncer: self announcer
]

{ #category : 'private - registration' }
RPackageOrganizer >> registerPackage: aPackage forClass: aClass [

	(aPackage includesClass: aClass) ifFalse: [ self error: aPackage name , ' does not includes the class ' , aClass name ].
	^ classPackageMapping at: aClass instanceSide put: aPackage
]

{ #category : 'system integration' }
RPackageOrganizer >> removeClass: class [
	"Remove the class, the class backpointer, the extensions and the extension backPointer from the receiver and the class involved with the class named: className. className is a class name and should not be a metaclass one. "

	class isClassSide ifTrue: [ self error: 'We should only be able to remove classes and not instances of metaclass.' ].
	(self packageOf: class) removeClass: class.
	(self extendingPackagesOf: class) do: [ :extendedPackage | extendedPackage removeAllMethodsFromClass: class ]
]

{ #category : 'cleanup' }
RPackageOrganizer >> removeEmptyPackagesAndTags [
	"Remove empty packages and tags."

	self packages do: [ :package | package removeEmptyTags ].
	(self packages select: [ :package | package isEmpty ]) do: [ :emptyPackage | self removePackage: emptyPackage ]
]

{ #category : 'registration' }
RPackageOrganizer >> removePackage: aPackage [

	self
		packageNamed: (aPackage isString
				 ifTrue: [ aPackage ]
				 ifFalse: [ aPackage name ])
		ifPresent: [ :package | package removeFromSystem ]
]

{ #category : 'system integration' }
RPackageOrganizer >> removeTag: aTag fromPackage: aPackage [

	(self hasPackage: aPackage) ifFalse: [ ^ self "no need to remove the tag if the package is not there." ].

	(self ensurePackage: aPackage) removeTag: aTag
]

{ #category : 'system integration' }
RPackageOrganizer >> renamePackage: aPackage to: newName [

	(self hasPackage: aPackage) ifFalse: [ ^ self ].

	(self ensurePackage: aPackage) renameTo: newName
]

{ #category : 'system integration' }
RPackageOrganizer >> renameTag: aTag to: newName inPackage: aPackage [

	(self hasPackage: aPackage) ifFalse: [ ^ self ].

	(self ensurePackage: aPackage) renameTag: aTag to: newName
]

{ #category : 'system integration' }
RPackageOrganizer >> systemClassAddedActionFrom: announcement [

	self systemClassRecategorizedActionFrom: announcement
]

{ #category : 'system integration' }
RPackageOrganizer >> systemClassRecategorizedActionFrom: announcement [

	| newPackage |
	newPackage := self ensurePackageMatching: announcement classAffected category.
	newPackage moveClass: announcement classAffected toTag: (newPackage toTagName: announcement classAffected category)
]

{ #category : #'system integration' }
RPackageOrganizer >> systemMethodAddedActionFrom: ann [

	| method |
	"If the method origin is not the one of the event, we do not care about that method"
	method := ann method.
	method origin = ann methodClass ifFalse: [ ^ self ].

	self addMethod: method
]

{ #category : 'system integration' }
RPackageOrganizer >> systemMethodRecategorizedActionFrom: ann [

	| method |
	method := ann method ifNil: [ ^ self ].

	method origin = ann methodClass ifFalse: [ ^ self ]. "Methods from traits are not treated"

	ann methodClass package isUndefined ifTrue: [ ^ self ]. "Classes that are not packages are not treated"

	"If the new protocol is nil it means we are removing the method and this will be managed by the method removal"
	ann newProtocol ifNotNil: [ :newProtocol |
		| oldPackage newPackage |
		newPackage := newProtocol isExtensionProtocol
			              ifTrue: [ self ensurePackageOfExtensionProtocol: newProtocol ]
			              ifFalse: [ method methodClass package ].

		oldPackage := (self packageForProtocol: ann oldProtocol) ifNil: [ method methodClass package ].

		oldPackage = newPackage ifTrue: [ ^ self ].

		oldPackage removeMethod: method.
		newPackage addMethod: method.

		SystemAnnouncer uniqueInstance methodRepackaged: method from: oldPackage to: newPackage ]
]

{ #category : 'accessing' }
RPackageOrganizer >> testPackageNames [

	^ self testPackages collect: [ :package | package name ]
]

{ #category : 'accessing' }
RPackageOrganizer >> testPackages [

	^ self packages select: [ :package | package isTestPackage ]
]

{ #category : 'accessing' }
RPackageOrganizer >> undefinedPackage [

	^ self packages detect: [ :package | package isUndefined ]
]

{ #category : 'initialization' }
RPackageOrganizer >> unregister [
	SystemAnnouncer uniqueInstance unsubscribe: self
]

{ #category : 'private - registration' }
RPackageOrganizer >> unregisterExtendingPackage: aPackage forClass: aClass [

	classExtendingPackagesMapping at: aClass instanceSide ifPresent: [ :extendingPackages | extendingPackages remove: aPackage ifAbsent: [  ] ]
]

{ #category : 'system integration' }
RPackageOrganizer >> unregisterInterestToSystemAnnouncement [
	"self unregisterInterestToSystemAnnouncement"

	SystemAnnouncer uniqueInstance unsubscribe: self
]

{ #category : 'private' }
RPackageOrganizer >> unregisterPackage: aPackage [
	"I am a private method to unregister a package from myself without removing its contents fro the image."

	| package |
	(self hasPackage: aPackage) ifFalse: [ ^ self ].

	package := aPackage isString
		           ifTrue: [ self packageNamed: aPackage ]
		           ifFalse: [ aPackage ].

	self basicUnregisterPackage: package.
	package extendedClasses do: [ :extendedClass | self unregisterExtendingPackage: package forClass: extendedClass ].
	package definedClasses do: [ :definedClass | self unregisterPackage: package forClass: definedClass ].
	SystemAnnouncer announce: (PackageRemoved to: package).

	^ package
]

{ #category : 'private - registration' }
RPackageOrganizer >> unregisterPackage: aPackage forClass: aClass [
	"unregister the back pointer mapping from classes to packages."

	self flag: #package. "When RPackage will be simplified we should not have to check the package but it is required for now"
	^ (classPackageMapping at: aClass instanceSide ifAbsent: [ ^ self ]) = aPackage ifTrue: [ classPackageMapping removeKey: aClass instanceSide ]
]

{ #category : 'registration' }
RPackageOrganizer >> validatePackageDoesNotExist: aPackage [

	| packageName |
	packageName := aPackage isString
		               ifTrue: [ aPackage ]
		               ifFalse: [ aPackage name ].
	self packageNamed: packageName ifPresent: [ RPackageConflictError signal: ('A package named {1} already exists' format: { packageName }) ]
]
