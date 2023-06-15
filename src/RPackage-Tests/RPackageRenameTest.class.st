"
SUnit tests on renaming packages
"
Class {
	#name : #RPackageRenameTest,
	#superclass : #TestCase,
	#instVars : [
		'oldSystemAnnouncer',
		'packageNamesToClean'
	],
	#category : #'RPackage-Tests'
}

{ #category : #utilities }
RPackageRenameTest >> cleanWorkingCopiesInTearDown: aCollection [

	packageNamesToClean addAll: aCollection
]

{ #category : #running }
RPackageRenameTest >> setUp [
	super setUp.
	oldSystemAnnouncer := SystemAnnouncer uniqueInstance.
	SystemAnnouncer restoreAllNotifications.
	packageNamesToClean := OrderedCollection new
]

{ #category : #running }
RPackageRenameTest >> tearDown [

	SystemAnnouncer announcer: oldSystemAnnouncer.
	self class environment at: #TestClass ifPresent: [ :class | class removeFromSystem ].
	self class environment at: #TestClass1 ifPresent: [ :class | class removeFromSystem ].
	self class environment at: #TestClass2 ifPresent: [ :class | class removeFromSystem ].
	self class environment at: #TestClass3 ifPresent: [ :class | class removeFromSystem ].

	packageNamesToClean do: [ :aPackageName | (MCWorkingCopy hasPackageNamed: aPackageName) ifTrue: [ (MCWorkingCopy forPackageNamed: aPackageName) unload ] ].

	super tearDown
]

{ #category : #tests }
RPackageRenameTest >> testRenamePackage [
	"Test that we do rename the package as expected."

	| package workingCopy class |
	self cleanWorkingCopiesInTearDown: #( 'TestRename' ).

	package := self packageOrganizer registerPackageNamed: 'Test1'.
	workingCopy := MCWorkingCopy forPackageNamed: 'Test1'.
	class := (Object << #TestClass package: 'Test1-TAG') install.

	self assert: (package includesClass: class).
	self assert: (package classTagNamed: #TAG ifAbsent: [ nil ]) notNil.
	self assert: ((package classTagNamed: #TAG ifAbsent: [ nil ]) includesClass: class).
	self assert: workingCopy modified.

	package renameTo: 'TestRename'.

	self assert: (package includesClass: class).
	self assert: (package classTagNamed: #TAG ifAbsent: [ nil ]) notNil.
	self assert: ((package classTagNamed: #TAG ifAbsent: [ nil ]) includesClass: class).
	self assert: class category equals: #'TestRename-TAG'.
	self deny: (self packageOrganizer includesCategory: #Test1).
	self deny: (MCWorkingCopy hasPackageNamed: #Test1).

	workingCopy := MCWorkingCopy forPackageNamed: 'TestRename'.
	self assert: workingCopy modified
]

{ #category : #tests }
RPackageRenameTest >> testRenamePackageToOwnTagName [
	"If we rename a package to the (full)category name of one of its tags"

	| package workingCopy class1 class2 |
	self cleanWorkingCopiesInTearDown: #( 'Test1-Core' ).

	package := self packageOrganizer registerPackageNamed: 'Test1'.
	workingCopy := MCWorkingCopy forPackageNamed: 'Test1'.
	class1 := (Object << #TestClass1 package: 'Test1-Core') install.
	class2 := (Object << #TestClass2 package: 'Test1-Util') install.

	self assert: (package classTagNamed: #Core ifAbsent: [ nil ]) notNil.
	self assert: (package classTagNamed: #Util ifAbsent: [ nil ]) notNil.
	package renameTo: 'Test1-Core'.
	self assert: (package includesClass: class1).
	self assert: (package includesClass: class2).
	self assert: (package classTagNamed: #Core ifAbsent: [ nil ]) notNil.
	self assert: (package classTagNamed: #Util ifAbsent: [ nil ]) notNil.
	self assert: ((package classTagNamed: #Core ifAbsent: [ nil ]) includesClass: class1).
	self assert: ((package classTagNamed: #Util ifAbsent: [ nil ]) includesClass: class2).

	workingCopy := MCWorkingCopy forPackageNamed: 'Test1-Core'.
	self assert: workingCopy modified
]

{ #category : #tests }
RPackageRenameTest >> testRenamePackageToOwnTagNameWithClassesInRoot [
	"If we rename a package to the (full)category name of one of its tags and the (non-tag)package is not empty"

	| package workingCopy class1 class2 class3 |
	self cleanWorkingCopiesInTearDown: #( 'Test1-Core' ).

	package := self packageOrganizer registerPackageNamed: 'Test1'.
	workingCopy := MCWorkingCopy forPackageNamed: 'Test1'.
	class1 := (Object << #TestClass1 package: 'Test1-Core') install.
	class2 := (Object << #TestClass2 package: 'Test1-Util') install.
	class3 := (Object << #TestClass3 package: 'Test1') install.

	self assert: (package classTagNamed: #Core ifAbsent: [ nil ]) notNil.
	self assert: (package classTagNamed: #Util ifAbsent: [ nil ]) notNil.
	self assert: (package classTagForClass: class1) name equals: #Core.
	self assert: (package classTagForClass: class2) name equals: #Util.
	self assert: (package classTagForClass: class3) name equals: #Test1.
	package renameTo: 'Test1-Core'.
	self assert: (package classTagForClass: class1) name equals: #Core.
	self assert: (package classTagForClass: class2) name equals: #Util.
	self assert: (package classTagForClass: class3) name equals: #'Test1-Core'.

	workingCopy := MCWorkingCopy forPackageNamed: 'Test1-Core'.
	self assert: workingCopy modified
]

{ #category : #tests }
RPackageRenameTest >> testRenamePackageUppercase [
	"Test that we do rename the package as expected."

	| package pkg oldWorkingCopy |
	"preparation: creation of a pkg and its associated mcworkingcopy"
	[
	package := self packageOrganizer registerPackageNamed: 'Test1'.
	oldWorkingCopy := MCWorkingCopy forPackageNamed: 'Test1'.
	self assert: (self packageOrganizer includesPackageNamed: #Test1).
	self assert: (MCWorkingCopy hasPackageNamed: #Test1) isNotNil.

	"renaming"
	package renameTo: 'TEST1'.

	pkg := self packageOrganizer packageNamed: 'Test1' ifAbsent: [ nil ].
	self assert: pkg isNil.
	self assert: 'TEST1' asPackage isNotNil.
	self deny: 'TEST1' asPackage mcWorkingCopy equals: oldWorkingCopy.
	self assert: 'TEST1' asPackage mcWorkingCopy isNotNil.
	self assert: (self packageOrganizer includesPackageNamed: #TEST1).
	self assert: (MCWorkingCopy hasPackageNamed: #TEST1) isNotNil ] ensure: [ "cleaning"
		'TEST1' asPackage removeFromSystem.
		self deny: (self packageOrganizer includesPackageNamed: #TEST1).
		self deny: (MCWorkingCopy hasPackageNamed: #TEST1).
		self deny: (self packageOrganizer includesPackageNamed: #Test1).
		self deny: (MCWorkingCopy hasPackageNamed: #Test1) ]
]

{ #category : #tests }
RPackageRenameTest >> testRenamePackageWithExtensions [

	| package workingCopy class extendedPackage extendedPackageWorkingCopy extension |
	package := self packageOrganizer registerPackageNamed: 'OriginalPackage'.
	workingCopy := MCWorkingCopy forPackageNamed: 'OriginalPackage'.

	class := (Object << #TestClass package: 'OriginalPackage-TAG') install.

	extendedPackage := self packageOrganizer registerPackageNamed: 'ExtendedPackage'.
	extendedPackageWorkingCopy := MCWorkingCopy forPackageNamed: 'ExtendedPackage'.

	self cleanWorkingCopiesInTearDown: #( ExtendedPackage RenamedPackage OriginalPackage ).

	class := (Object << #TestExtendedClass package: 'ExtendedPackage') install.

	class compile: 'm ^ 42' classified: '*OriginalPackage'.
	extension := class >> #m.

	self assert: extension isExtension.
	self assert: extension package equals: package.
	self assert: extension origin equals: class.
	self assert: class package equals: extendedPackage.

	package renameTo: 'RenamedPackage'.

	extension := class >> #m.

	self assert: extension isExtension.
	self assert: extension package equals: package.
	self assert: extension origin equals: class.
	self assert: class package equals: extendedPackage
]

{ #category : #tests }
RPackageRenameTest >> testRenamePackageWithExtensionsInClassSide [

	| package workingCopy class extendedPackage extendedPackageWorkingCopy extension |
	package := self packageOrganizer registerPackageNamed: 'OriginalPackage'.
	workingCopy := MCWorkingCopy forPackageNamed: 'OriginalPackage'.

	class := (Object << #TestClass package: 'OriginalPackage-TAG') install.

	extendedPackage := self packageOrganizer registerPackageNamed: 'ExtendedPackage'.
	extendedPackageWorkingCopy := MCWorkingCopy forPackageNamed: 'ExtendedPackage'.

	self cleanWorkingCopiesInTearDown: #( ExtendedPackage RenamedPackage OriginalPackage ).

	class := (Object << #TestExtendedClass package: 'ExtendedPackage') install.

	class := class class.

	class compile: 'm ^ 42' classified: '*OriginalPackage'.
	extension := class >> #m.

	self assert: extension isExtension.
	self assert: extension package equals: package.
	self assert: extension origin equals: class.
	self assert: class package equals: extendedPackage.

	package renameTo: 'RenamedPackage'.

	extension := class >> #m.

	self assert: extension isExtension.
	self assert: extension package equals: package.
	self assert: extension origin equals: class.
	self assert: class package equals: extendedPackage
]

{ #category : #tests }
RPackageRenameTest >> testUnregisterPackage [
	"Test that we do unregister the package as expected."

	| package workingCopy class |
	package := self packageOrganizer registerPackageNamed: 'Test1'.
	workingCopy := MCWorkingCopy forPackageNamed: 'Test1'.
	class := (Object << #TestClass package: 'Test1-TAG') install.
	self assert: (package includesClass: class).
	self assert: (package classTagNamed: #TAG ifAbsent: [ nil ]) notNil.
	self assert: ((package classTagNamed: #TAG ifAbsent: [ nil ]) includesClass: class).
	self assert: workingCopy modified.

	package unregister.

	self deny: (self packageOrganizer includesPackageNamed: #Test1).
	self deny: (MCWorkingCopy hasPackageNamed: #Test1)
]
