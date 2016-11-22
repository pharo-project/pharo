tests
testRenamePackageToOwnTagNameWithClassesInRoot
	"If we rename a package to the (full)category name of one of its tags and the (non-tag)package is not empty"
	| package workingCopy class1 class2 class3|

	package := RPackageOrganizer default registerPackageNamed: 'Test1'.
	workingCopy := MCWorkingCopy forPackage: (MCPackage new name: 'Test1').
	class1 := Object subclass: #TestClass1 instanceVariableNames: '' classVariableNames: '' package: 'Test1-Core'.
	class2 := Object subclass: #TestClass2 instanceVariableNames: '' classVariableNames: '' package: 'Test1-Util'.
	class3 := Object subclass: #TestClass3 instanceVariableNames: '' classVariableNames: '' package: 'Test1'.
	
	self assert: (package classTagNamed: #Core ifAbsent: [ nil ]) notNil.
	self assert: (package classTagNamed: #Util ifAbsent: [ nil ]) notNil.
	self assert: (package classTagForClass: class1) name equals: #Core.
	self assert: (package classTagForClass: class2) name equals: #Util.
	self assert: (package classTagForClass: class3) name equals: #Test1.
	package renameTo: 'Test1-Core'.
	self assert: (package classTagForClass: class1) name equals: #Core.
	self assert: (package classTagForClass: class2) name equals: #Util.
	self assert: (package classTagForClass: class3) name equals: #'Test1-Core'.
	"cleaning"
	workingCopy := MCWorkingCopy forPackage: (MCPackage new name: 'Test1-Core').
	self assert: workingCopy modified.
	workingCopy unload.	
	self deny: (RPackageOrganizer default includesPackageNamed: #'Test1-Core').
	self deny: (MCWorkingCopy hasPackageNamed: #'Test1-Core').
