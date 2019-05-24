Class {
	#name : #RBChildrenToSiblingsTest,
	#superclass : #RBRefactoringTest,
	#category : #'Refactoring-Tests-Core'
}

{ #category : #running }
RBChildrenToSiblingsTest >> setUp [
	super setUp.
	model := Smalltalk compiler evaluate: self childrenToSiblingTestData
]

{ #category : #'failure tests' }
RBChildrenToSiblingsTest >> testBadName [
	self shouldFail: (RBChildrenToSiblingsRefactoring 
			name: #'Obje ct'
			class: RBLintRuleTestData
			subclasses: (Array 
					with: RBBasicLintRuleTestData
					with: RBCompositeLintRuleTestData))
]

{ #category : #'failure tests' }
RBChildrenToSiblingsTest >> testExistingName [
	self
		shouldFail: (RBChildrenToSiblingsRefactoring 
				name: #Object
				class: RBLintRuleTestData
				subclasses: (Array 
						with: RBBasicLintRuleTestData
						with: RBCompositeLintRuleTestData));
		shouldFail: (RBChildrenToSiblingsRefactoring 
				name: #Processor
				class: RBLintRuleTestData
				subclasses: (Array 
						with: RBBasicLintRuleTestData
						with: RBCompositeLintRuleTestData))
]

{ #category : #'failure tests' }
RBChildrenToSiblingsTest >> testInvalidSubclass [
	self shouldFail: (RBChildrenToSiblingsRefactoring 
			name: #Foo
			class: RBRefactoringTest
			subclasses: (Array 
					with: RBBasicLintRuleTestData
					with: RBCompositeLintRuleTestData))
]

{ #category : #'failure tests' }
RBChildrenToSiblingsTest >> testMetaClassFailure [
	self shouldFail: (RBChildrenToSiblingsRefactoring 
			name: #Foo
			class: RBLintRuleTestData class
			subclasses: (Array 
					with: RBBasicLintRuleTestData class
					with: RBCompositeLintRuleTestData class))
]

{ #category : #tests }
RBChildrenToSiblingsTest >> testModelChildrenToSibling [
	| refactoring class subclass superclass |
	class := model classNamed: #ConcreteSuperclass.
	subclass := model classNamed: #ConcreteSubclass.
	refactoring := RBChildrenToSiblingsRefactoring 
		model: model
		name: #AbstractSuperclass
		class: class
		subclasses: (Array with: subclass).
	self executeRefactoring: refactoring.
	superclass := refactoring model classNamed: #AbstractSuperclass.
	self assert: class superclass = superclass.
	self assert: class classSide superclass = superclass classSide.
	self assert: subclass superclass = superclass.
	self assert: subclass classSide superclass = superclass classSide.
	self assert: (superclass parseTreeFor: #same) = (self parseMethod: 'same ^self initialize isKindOf: AbstractSuperclass').
	self assert: (superclass parseTreeFor: #different) = (self parseMethod: 'different self subclassResponsibility').
	self assert: (superclass parseTreeFor: #initialize) = (self parseMethod: 'initialize
							instVarName1 := instVarName2 := ClassVarName1 := ClassVarName2 := 0').
	self assert: (superclass directlyDefinesInstanceVariable: 'instVarName1').
	self assert: (superclass directlyDefinesInstanceVariable: 'instVarName2').
	self assert: (superclass directlyDefinesClassVariable: 'ClassVarName1').
	self assert: (superclass directlyDefinesClassVariable: 'ClassVarName2').
	self assert: (superclass classSide directlyDefinesInstanceVariable: 'classInstVarName1').
	self assert: (superclass classSide parseTreeFor: #foo) = (self parseMethod: 'foo
							^classInstVarName1 + ClassVarName1 + ClassVarName2').
	self assert: (superclass classSide parseTreeFor: #new) = (self parseMethod: 'new
							^super new initialize').
	self assert: (superclass classSide parseTreeFor: #bar) = (self parseMethod: 'bar
							self subclassResponsibility').
	self deny: (class directlyDefinesInstanceVariable: 'instVarName1').
	self deny: (class directlyDefinesInstanceVariable: 'instVarName2').
	self deny: (class directlyDefinesClassVariable: 'ClassVarName1').
	self deny: (class directlyDefinesClassVariable: 'ClassVarName2').
	self deny: (class classSide directlyDefinesInstanceVariable: 'classInstVarName1').
	self deny: (class directlyDefinesMethod: #same).
	self deny: (class directlyDefinesMethod: #initialize).
	self deny: (class classSide directlyDefinesMethod: #new).
	self assert: (class parseTreeFor: #different) = (self parseMethod: 'different
							^instVarName1 + instVarName2').
	self assert: (class classSide parseTreeFor: #bar) = (self parseMethod: 'bar
							^self printString')
]
