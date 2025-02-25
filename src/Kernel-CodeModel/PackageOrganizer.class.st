"
I am responsible for providing all the package currently defined in an environment.

The system currently hold a global environment with a default package organizer but it is possible to create new enviornemnt with `SystemEnvironment new` and they will come with a new package organizer.

I am used as en entry point to access packages and tags and update the model.
"
Class {
	#name : 'PackageOrganizer',
	#superclass : 'Object',
	#instVars : [
		'packages',
		'environment'
	],
	#category : 'Kernel-CodeModel-Packages',
	#package : 'Kernel-CodeModel',
	#tag : 'Packages'
}

{ #category : 'singleton' }
PackageOrganizer class >> default [
	"Warning: Use 'self packageOrganizer' since I will be removed in the future."

	^ self packageOrganizer
]

{ #category : 'registration' }
PackageOrganizer >> addPackage: aPackage [

	| package |
	self validatePackageDoesNotExist: aPackage.

	package := aPackage isString
		           ifTrue: [ Package named: aPackage organizer: self ]
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

	^ environment ifNil: [ environment := self class environment ]
]

{ #category : 'accessing' }
PackageOrganizer >> environment: aSystemEnvironment [
	 environment := aSystemEnvironment
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
	self packageNamed: packageName ifPresent: [ PackageConflictError signal: ('A package named {1} already exists' format: { packageName }) ]
]
