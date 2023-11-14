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
	#name : 'PackageOrganizer',
	#superclass : 'Object',
	#instVars : [
		'packages',
		'environment'
	],
	#category : 'RPackage-Core-Base',
	#package : 'RPackage-Core',
	#tag : 'Base'
}

{ #category : 'singleton' }
PackageOrganizer class >> default [
	"Warning: Use 'self packageOrganizer' since I will be removed in the future."

	^ self packageOrganizer
]

{ #category : 'class initialization' }
PackageOrganizer class >> initialize [

	self deprecatedAliases: { #RPackageOrganizer }
]

{ #category : 'registration' }
PackageOrganizer >> addPackage: aPackage [

	| package |
	self validatePackageDoesNotExist: aPackage.

	package := aPackage isString
		           ifTrue: [ RPackage named: aPackage organizer: self ]
		           ifFalse: [ aPackage organizer: self ].

	self basicRegisterPackage: package.

	self codeChangeAnnouncer  announce: (PackageAdded to: package).

	^ package
]

{ #category : 'private - registration' }
PackageOrganizer >> basicRegisterPackage: aPackage [
	"A new package is now available and declared in the receiver. Note that it is a low level implementation method since it does not deal with package contained information and does not update the related mapping tables."

	^ packages at: aPackage name asSymbol put: aPackage
]

{ #category : 'private - registration' }
PackageOrganizer >> basicUnregisterPackage: aPackage [
	"Unregister the specified package from the list of registered packages. Raise the PackageRemoved announcement. This is a low level action. It does not unregister the back pointer from classes to packages or any other information managed by the organizer"

	^ packages removeKey: aPackage name ifAbsent: [  ]
]

{ #category : 'accessing' }
PackageOrganizer >> codeChangeAnnouncer [

	^ self environment codeChangeAnnouncer
]

{ #category : 'registration' }
PackageOrganizer >> ensurePackage: aPackage [

	^ self packageNamed: (aPackage isString ifTrue: [ aPackage ] ifFalse: [ aPackage name ]) ifAbsent: [ self addPackage: aPackage ]
]

{ #category : 'registration' }
PackageOrganizer >> ensurePackage: aPackage withTags: aCollection [

	| package |
	package := self ensurePackage: aPackage.
	aCollection do: [ :tag | package ensureTag: tag ]
]

{ #category : 'system integration' }
PackageOrganizer >> ensurePackageMatching: aString [
	"I try to find a package matching the string. The priority is given to a package having the same name case insensitive. If I don't find it, I'm stripping everything following the last dash and I retry to find the package. If non is found after all dashes are stripped, I create a package of the given name.
	
	This is useful when trying to find a package from a protocol name since we can add some dashes to protocol names after the package name. Or to find a package fron it's category until the categories are removed from the system."

	^ (self packageMatchingExtensionName: aString) ifNil: [ self ensurePackage: aString ]
]

{ #category : 'system integration' }
PackageOrganizer >> ensurePackageOfExtensionProtocol: aProtocol [

	^ (self packageForProtocol: aProtocol) ifNil: [ self ensurePackage: aProtocol name allButFirst capitalized ]
]

{ #category : 'registration' }
PackageOrganizer >> ensureTag: aTagName inPackage: aPackageName [

	^ (self ensurePackage: aPackageName) ensureTag: aTagName
]

{ #category : 'accessing' }
PackageOrganizer >> environment [
	 ^ environment ifNil: [ environment := Smalltalk globals]
]

{ #category : 'accessing' }
PackageOrganizer >> environment: aSystemDictionary [
	 environment := aSystemDictionary
]

{ #category : 'testing' }
PackageOrganizer >> hasPackage: aPackage [
	"Takes a package or a package name as parameter and return true if I include this package."

	^ aPackage isString
		  ifTrue: [
			  self packageNamed: aPackage asSymbol ifAbsent: [ ^ false ].
			  ^ true ]
		  ifFalse: [ self packages includes: aPackage ]
]

{ #category : 'testing' }
PackageOrganizer >> hasTag: aTag inPackage: aPackage [

	(self hasPackage: aPackage) ifFalse: [ ^ false ].

	^ (self ensurePackage: aPackage) hasTag: aTag
]

{ #category : 'initialization' }
PackageOrganizer >> initialize [

	super initialize.

	packages := IdentityDictionary new.
	self ensurePackage: UndefinedPackage new
]

{ #category : 'testing' }
PackageOrganizer >> isClassInstalled: aClass [

	(self hasPackage: aClass package) ifFalse: [ ^ false ].

	"It could be that the class got removed but still knows its package"
	(aClass package includesClass: aClass) ifFalse: [ ^ false ].

	^ aClass package isUndefined not
]

{ #category : 'system integration' }
PackageOrganizer >> packageForProtocol: aProtocol [
	"Return nil if the protocol is not an extension of if there is no package matching the name.
	
	Else return the package matching the protocol. The priority is given to a package with the same name as the protocol or a package whose is a prefix of the protocol followed by a dash.
	I work in a case insensitive way."

	"If the protocol is nil or isnt an extension protocol then we have the package since it's the same as the class."
	(aProtocol isNil or: [ aProtocol isExtensionProtocol not ]) ifTrue: [ ^ nil ].

	^ self packageMatchingExtensionName: (aProtocol name copyWithout: $*)
]

{ #category : 'private' }
PackageOrganizer >> packageForProtocol: protocol from: aClass [

	^ (protocol isNotNil and: [ protocol isExtensionProtocol ])
		  ifTrue: [ self ensurePackageOfExtensionProtocol: protocol ]
		  ifFalse: [ aClass package ]
]

{ #category : 'system integration' }
PackageOrganizer >> packageMatchingExtensionName: anExtensionName [
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
PackageOrganizer >> packageNamed: aSymbol [
	^ self
		packageNamed: aSymbol
		ifAbsent: [ KeyNotFound signalFor: aSymbol ]
]

{ #category : 'accessing' }
PackageOrganizer >> packageNamed: aSymbol ifAbsent: errorBlock [
	"We first look at the fast solution then we try the case insensitive way because we do not care about the case in this package manager."

	^ packages at: aSymbol asSymbol ifAbsent: [
		  self packagesDo: [ :each | (each name sameAs: aSymbol) ifTrue: [ ^ each ] ].
		  errorBlock value ]
]

{ #category : 'accessing' }
PackageOrganizer >> packageNamed: aSymbol ifPresent: aBlock [
	"We first look at the fast solution then we try the case insensitive way because we do not care about the case in this package manager."

	^ packages
		  at: aSymbol asSymbol
		  ifPresent: aBlock
		  ifAbsent: [
			  self packagesDo: [ :each | (each name sameAs: aSymbol) ifTrue: [ ^ aBlock cull: each ] ].
			  nil ]
]

{ #category : 'package - names-cache' }
PackageOrganizer >> packageNames [

	^ packages keys
]

{ #category : 'package - names-cache' }
PackageOrganizer >> packageNamesDo: aBlock [

	^ self packageNames do: aBlock
]

{ #category : 'package - access from class' }
PackageOrganizer >> packageOf: aClass [

	^ self packages
		  detect: [ :package | package includesClass: aClass ]
		  ifNone: [ self undefinedPackage ]
]

{ #category : 'package - access from class' }
PackageOrganizer >> packageOfClassNamed: aName [

	^ self packages
		  detect: [ :package | package includesClassNamed: aName ]
		  ifNone: [ self undefinedPackage ]
]

{ #category : 'accessing' }
PackageOrganizer >> packages [

	^ packages values
]

{ #category : 'accessing' }
PackageOrganizer >> packagesDo: aBlock [

	self packages do: aBlock
]

{ #category : 'system integration' }
PackageOrganizer >> removeClass: class [
	"Remove the class, the class backpointer, the extensions and the extension backPointer from the receiver and the class involved with the class named: className. className is a class name and should not be a metaclass one. "

	class isClassSide ifTrue: [ self error: 'We should only be able to remove classes and not instances of metaclass.' ].
	class package removeClass: class.
	class extendingPackages do: [ :extendedPackage | extendedPackage removeAllExtensionMethodsFromClass: class ]
]

{ #category : 'cleanup' }
PackageOrganizer >> removeEmptyPackagesAndTags [
	"Remove empty packages and tags."

	self packages do: [ :package | package removeEmptyTags ].
	(self packages select: [ :package | package isEmpty ]) do: [ :emptyPackage | self removePackage: emptyPackage ]
]

{ #category : 'registration' }
PackageOrganizer >> removePackage: aPackage [

	self
		packageNamed: (aPackage isString
				 ifTrue: [ aPackage ]
				 ifFalse: [ aPackage name ])
		ifPresent: [ :package | package removeFromSystem ]
]

{ #category : 'system integration' }
PackageOrganizer >> removeTag: aTag fromPackage: aPackage [

	(self hasPackage: aPackage) ifFalse: [ ^ self "no need to remove the tag if the package is not there." ].

	(self ensurePackage: aPackage) removeTag: aTag
]

{ #category : 'system integration' }
PackageOrganizer >> renamePackage: aPackage to: newName [

	(self hasPackage: aPackage) ifFalse: [ ^ self ].

	(self ensurePackage: aPackage) renameTo: newName
]

{ #category : 'system integration' }
PackageOrganizer >> renameTag: aTag to: newName inPackage: aPackage [

	(self hasPackage: aPackage) ifFalse: [ ^ self ].

	(self ensurePackage: aPackage) renameTag: aTag to: newName
]

{ #category : 'system integration' }
PackageOrganizer >> repackageMethod: method oldProtocol: oldProtocol newProtocol: newProtocol [

	| oldPackage newPackage |
	newPackage := self packageForProtocol: newProtocol from: method methodClass.
	oldPackage := self packageForProtocol: oldProtocol from: method methodClass.

	oldPackage = newPackage ifTrue: [ ^ self ].

	oldPackage removeMethod: method.
	newPackage addMethod: method.

	self codeChangeAnnouncer  methodRepackaged: method from: oldPackage to: newPackage
]

{ #category : 'accessing' }
PackageOrganizer >> testPackageNames [

	^ self testPackages collect: [ :package | package name ]
]

{ #category : 'accessing' }
PackageOrganizer >> testPackages [

	^ self packages select: [ :package | package isTestPackage ]
]

{ #category : 'accessing' }
PackageOrganizer >> undefinedPackage [

	^ self packages detect: [ :package | package isUndefined ]
]

{ #category : 'private' }
PackageOrganizer >> unregisterPackage: aPackage [
	"I am a private method to unregister a package from myself without removing its contents fro the image."

	| package |
	(self hasPackage: aPackage) ifFalse: [ ^ self ].

	package := aPackage isString
		           ifTrue: [ self packageNamed: aPackage ]
		           ifFalse: [ aPackage ].

	self basicUnregisterPackage: package.
	self codeChangeAnnouncer  announce: (PackageRemoved to: package).

	^ package
]

{ #category : 'registration' }
PackageOrganizer >> validatePackageDoesNotExist: aPackage [

	| packageName |
	packageName := aPackage isString
		               ifTrue: [ aPackage ]
		               ifFalse: [ aPackage name ].
	self packageNamed: packageName ifPresent: [ RPackageConflictError signal: ('A package named {1} already exists' format: { packageName }) ]
]
