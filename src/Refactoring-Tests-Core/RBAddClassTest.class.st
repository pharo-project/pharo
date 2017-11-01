Class {
	#name : #RBAddClassTest,
	#superclass : #RBRefactoringTest,
	#category : 'Refactoring-Tests-Core'
}

{ #category : #running }
RBAddClassTest >> setUp [
	super setUp.
	model := self abstractVariableTestData.
]

{ #category : #tests }
RBAddClassTest >> testAddClass [
	| refactoring newClass superClass classTest |
	refactoring := RBAddClassRefactoring 
		addClass: #FooTest
		superclass: RBRefactoringTest
		subclasses: (Array with: self class)
		category: #'Refactory-Testing'.
	self executeRefactoring: refactoring.
	newClass := refactoring model classNamed: #FooTest.
	superClass := refactoring model classNamed: #RBRefactoringTest.
	classTest := refactoring model classNamed: self class name.
	self assert: newClass superclass = superClass.
	self assert: (superClass subclasses includes: newClass).
	self assert: newClass theMetaClass superclass = superClass theMetaClass.
	self assert: (superClass theMetaClass subclasses includes: newClass theMetaClass).
	self assert: classTest superclass = newClass.
	self assert: (newClass subclasses includes: classTest).
	self assert: classTest theMetaClass superclass = newClass theMetaClass.
	self assert: (newClass theMetaClass subclasses includes: classTest theMetaClass)
]

{ #category : #'failure tests' }
RBAddClassTest >> testExistingName [
	self shouldFail: (RBAddClassRefactoring 
				addClass: #Object
				superclass: self class
				subclasses: #()
				category: #'Refactory-Testing')
]

{ #category : #'failure tests' }
RBAddClassTest >> testInvalidSubclass [
	self shouldFail: (RBAddClassRefactoring 
			addClass: #Foo
			superclass: RBCompositeLintRuleTest
			subclasses: (Array with: RBBasicLintRuleTest)
			category: #'Refactory-Tesing')
]

{ #category : #'failure tests' }
RBAddClassTest >> testMetaClassFailure [
	self shouldFail: (RBAddClassRefactoring 
				addClass: #Foo
				superclass: self class class
				subclasses: #()
				category: #'Refactory-Testing')
]

{ #category : #tests }
RBAddClassTest >> testModelAddClass [
	| refactoring newClass superClass subclass |
	subclass := model classNamed: #Bar.
	superClass := model classNamed: #Foo.
	refactoring := RBAddClassRefactoring 
		model: model
		addClass: #FooTest
		superclass: superClass
		subclasses: (Array with: subclass)
		category: #'Refactory-Testing'.
	self executeRefactoring: refactoring.
	newClass := model classNamed: #FooTest.
	self assert: newClass superclass = superClass.
	self assert: (superClass subclasses includes: newClass).
	self assert: newClass theMetaClass superclass = superClass theMetaClass.
	self assert: (superClass theMetaClass subclasses includes: newClass theMetaClass).
	self assert: subclass superclass = newClass.
	self assert: (newClass subclasses includes: subclass).
	self assert: subclass theMetaClass superclass = newClass theMetaClass.
	self assert: (newClass theMetaClass subclasses includes: subclass theMetaClass)
]

{ #category : #'failure tests' }
RBAddClassTest >> testModelExistingName [
	| refactoring |
	refactoring := RBAddClassRefactoring 
				model: model
				addClass: #Foo
				superclass: Object
				subclasses: #()
				category: #'Refactory-Testing'.
	self shouldFail: refactoring
]

{ #category : #'failure tests' }
RBAddClassTest >> testModelInvalidSubclass [
	| refactoring |
	refactoring := RBAddClassRefactoring 
				model: model
				addClass: #Foo2
				superclass: Object
				subclasses: (Array with: (model classNamed: #Bar))
				category: #'Refactory-Tesing'.
	self shouldFail: refactoring
]
