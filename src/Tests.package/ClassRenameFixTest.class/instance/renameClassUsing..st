tests
renameClassUsing: aBlock
	| createdClass foundClasses |
	originalName := self newUniqueClassName.
	createdClass := Object
		subclass: originalName
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'ClassRenameFix-GeneradClass'.
	newClassName := self newUniqueClassName.
	aBlock value: createdClass value: newClassName.
	self assert: (Smalltalk globals classNamed: originalName) isNil.
	self assert: (Smalltalk globals classNamed: newClassName) notNil.
	foundClasses := Smalltalk globals organization listAtCategoryNamed: 'ClassRenameFix-GeneradClass'.
	self assert: foundClasses notEmpty.
	self assert: (foundClasses includes: newClassName).
	self assert: createdClass name = newClassName