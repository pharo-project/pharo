tests
testRenamePackageToOwnTagName
	"If we rename a package to the (full)category name of one of its tags"
	| package workingCopy class1 class2 |

	package := RPackageOrganizer default registerPackageNamed: 'Test1'.
	workingCopy := MCWorkingCopy forPackage: (MCPackage new name: 'Test1').
	class1 := Object subclass: #TestClass1 instanceVariableNames: '' classVariableNames: '' package: 'Test1-Core'.
	class2 := Object subclass: #TestClass2 instanceVariableNames: '' classVariableNames: '' package: 'Test1-Util'.
	
	self assert: (package classTagNamed: #Core ifAbsent: [ nil ]) notNil.
	self assert: (package classTagNamed: #Util ifAbsent: [ nil ]) notNil.
	package renameTo: 'Test1-Core'.
	self assert: (package includesClass: class1).
	self assert: (package includesClass: class2).
	self assert: (package classTagNamed: #'Core' ifAbsent: [ nil ]) notNil.
	self assert: (package classTagNamed: #'Util' ifAbsent: [ nil ]) notNil.
	self assert: ((package classTagNamed: #'Core' ifAbsent: [ nil ]) includesClass: class1).
	self assert: ((package classTagNamed: #'Util' ifAbsent: [ nil ]) includesClass: class2).	
	"cleaning"
	workingCopy := MCWorkingCopy forPackage: (MCPackage new name: 'Test1-Core').
	self assert: workingCopy modified.
	workingCopy unload.	
	self deny: (RPackageOrganizer default includesPackageNamed: #'Test1-Core').
	self deny: (MCWorkingCopy hasPackageNamed: #'Test1-Core').
