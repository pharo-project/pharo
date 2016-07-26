tests
testRenamePackage
	"Test that we do rename the package as expected."
	| package workingCopy class |

	package := RPackageOrganizer default registerPackageNamed: 'Test1'.
	workingCopy := MCWorkingCopy forPackage: (MCPackage new name: 'Test1').
	class := Object
		subclass: #TestClass
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Test1-TAG'.
	self assert: (package includesClass: class).
	self assert: (package classTagNamed: #TAG ifAbsent: [ nil ]) notNil.
	self assert: ((package classTagNamed: #TAG ifAbsent: [ nil ]) includesClass: class).
	self assert: workingCopy modified.
	package renameTo: 'TestRename'.
	self assert: (package includesClass: class).
	self assert: (package classTagNamed: #TAG ifAbsent: [ nil ]) notNil.
	self assert: ((package classTagNamed: #TAG ifAbsent: [ nil ]) includesClass: class).
	self assert: class category = #'TestRename-TAG'.	
	self deny: (Smalltalk organization includesCategory: #Test1).
	self deny: (MCWorkingCopy hasPackageNamed: #Test1).
	
	"cleaning"
	workingCopy := MCWorkingCopy forPackage: (MCPackage new name: 'TestRename').
	self assert: workingCopy modified.
	workingCopy unload.	
	self deny: (RPackageOrganizer default includesPackageNamed: #TestRename).
	self deny: (MCWorkingCopy hasPackageNamed: #TestRename).
