"
SUnit tests on renaming packages
"
Class {
	#name : 'PackageRenameTest',
	#superclass : 'PackageTestCase',
	#category : 'RPackage-Tests',
	#package : 'RPackage-Tests'
}

{ #category : 'tests' }
PackageRenameTest >> testRenamePackage [
	"Test that we do rename the package as expected."

	| package workingCopy class |
	package := self ensurePackage: 'Test1'.
	workingCopy := MCWorkingCopy forPackageNamed: 'Test1'.
	class := self newClassNamed: #TestClass in: package tag: 'TAG'.

	self assert: (package includesClass: class).
	self assert: (package tagNamed: #TAG ifAbsent: [ nil ]) notNil.
	self assert: ((package tagNamed: #TAG ifAbsent: [ nil ]) includesClass: class).
	self assert: workingCopy modified.

	package renameTo: 'TestRename'.

	self assert: (package includesClass: class).
	self assert: (package tagNamed: #TAG ifAbsent: [ nil ]) notNil.
	self assert: ((package tagNamed: #TAG ifAbsent: [ nil ]) includesClass: class).
	self assert: class category equals: #'TestRename-TAG'.
	self deny: (self packageOrganizer hasPackage: #Test1).
	self deny: (MCWorkingCopy hasPackageNamed: #Test1).

	workingCopy := MCWorkingCopy forPackageNamed: 'TestRename'.
	self assert: workingCopy modified
]

{ #category : 'tests' }
PackageRenameTest >> testRenamePackageToOwnTagName [
	"If we rename a package to the (full)category name of one of its tags"

	| package workingCopy class1 class2 |
	package := self ensurePackage: 'Test1'.
	workingCopy := MCWorkingCopy forPackageNamed: 'Test1'.
	class1 := self newClassNamed: #TestClass1 in: package tag: 'Core'.
	class2 := self newClassNamed: #TestClass2 in: package tag: 'Util'.

	self assert: (package tagNamed: #Core ifAbsent: [ nil ]) notNil.
	self assert: (package tagNamed: #Util ifAbsent: [ nil ]) notNil.
	package renameTo: 'Test1-Core'.
	self assert: (package includesClass: class1).
	self assert: (package includesClass: class2).
	self assert: (package tagNamed: #Core ifAbsent: [ nil ]) notNil.
	self assert: (package tagNamed: #Util ifAbsent: [ nil ]) notNil.
	self assert: ((package tagNamed: #Core ifAbsent: [ nil ]) includesClass: class1).
	self assert: ((package tagNamed: #Util ifAbsent: [ nil ]) includesClass: class2).

	workingCopy := MCWorkingCopy forPackageNamed: 'Test1-Core'.
	self assert: workingCopy modified
]

{ #category : 'tests' }
PackageRenameTest >> testRenamePackageToOwnTagNameWithClassesInRoot [
	"If we rename a package to the (full)category name of one of its tags and the (non-tag)package is not empty"

	| package workingCopy class1 class2 class3 |
	package := self ensurePackage: 'Test1'.
	class1 := self newClassNamed: #TestClass1 in: package tag: 'Core'.
	class2 := self newClassNamed: #TestClass2 in: package tag: 'Util'.
	class3 := self newClassNamed: #TestClass3 in: package.

	self assert: (package tagNamed: #Core ifAbsent: [ nil ]) isNotNil.
	self assert: (package tagNamed: #Util ifAbsent: [ nil ]) isNotNil.
	self assert: (package tagOf: class1) name equals: #Core.
	self assert: (package tagOf: class2) name equals: #Util.
	self assert: (package tagOf: class3) isRoot.
	package renameTo: 'Test1-Core'.
	self assert: (package tagOf: class1) name equals: #Core.
	self assert: (package tagOf: class2) name equals: #Util.
	self assert: (package tagOf: class3) isRoot.

	workingCopy := MCWorkingCopy forPackageNamed: 'Test1-Core'.
	self assert: workingCopy modified
]

{ #category : 'tests' }
PackageRenameTest >> testRenamePackageWithExtensions [

	| package class extendedPackage extension |
	package := self ensurePackage: 'OriginalPackage'.

	class := self newClassNamed: #TestClass in: package tag: 'TAG'.

	extendedPackage := self ensurePackage: 'ExtendedPackage'.


	class := self newClassNamed: #TestExtendedClass in: extendedPackage.

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

{ #category : 'tests' }
PackageRenameTest >> testRenamePackageWithExtensionsInClassSide [

	| package class extendedPackage extension |
	package := self ensurePackage: 'OriginalPackage'.
	class := self newClassNamed: #TestClass in: package tag: 'TAG'.
	extendedPackage := self ensurePackage: 'ExtendedPackage'.
	class := self newClassNamed: #TestExtendedClass in: extendedPackage.
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

{ #category : 'tests' }
PackageRenameTest >> testUnregisterPackage [
	"Test that we do unregister the package as expected."

	| package workingCopy class |
	self flag: #package. "Why is this here? It should be in Monticello tests!"
	package := self ensurePackage: 'Test1'.

	workingCopy := MCWorkingCopy forPackageNamed: 'Test1'.
	class := self newClassNamed: #TestClass in: package tag: 'TAG'.
	self assert: (package includesClass: class).
	self assert: (package tagNamed: #TAG ifAbsent: [ nil ]) notNil.
	self assert: ((package tagNamed: #TAG ifAbsent: [ nil ]) includesClass: class).
	self assert: workingCopy modified.

	package removeFromSystem.

	self deny: (self organizer hasPackage: #Test1).
	self deny: (MCWorkingCopy hasPackageNamed: #Test1)
]
