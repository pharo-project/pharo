tests
testUnregisterPackage
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
	
	package unregister.
	
	self deny: (RPackageOrganizer default includesPackageNamed: #Test1).
	self deny: (MCWorkingCopy hasPackageNamed: #Test1).
	