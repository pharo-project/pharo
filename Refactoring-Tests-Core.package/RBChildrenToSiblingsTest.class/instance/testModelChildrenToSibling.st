tests
testModelChildrenToSibling
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
	self assert: class theMetaClass superclass = superclass theMetaClass.
	self assert: subclass superclass = superclass.
	self assert: subclass theMetaClass superclass = superclass theMetaClass.
	self assert: (superclass parseTreeFor: #same) = (RBParser parseMethod: 'same ^self initialize isKindOf: AbstractSuperclass').
	self assert: (superclass parseTreeFor: #different) = (RBParser parseMethod: 'different self subclassResponsibility').
	self assert: (superclass parseTreeFor: #initialize) = (RBParser parseMethod: 'initialize
							instVarName1 := instVarName2 := ClassVarName1 := ClassVarName2 := 0').
	self assert: (superclass directlyDefinesInstanceVariable: 'instVarName1').
	self assert: (superclass directlyDefinesInstanceVariable: 'instVarName2').
	self assert: (superclass directlyDefinesClassVariable: 'ClassVarName1').
	self assert: (superclass directlyDefinesClassVariable: 'ClassVarName2').
	self assert: (superclass theMetaClass directlyDefinesInstanceVariable: 'classInstVarName1').
	self assert: (superclass theMetaClass parseTreeFor: #foo) = (RBParser parseMethod: 'foo
							^classInstVarName1 + ClassVarName1 + ClassVarName2').
	self assert: (superclass theMetaClass parseTreeFor: #new) = (RBParser parseMethod: 'new
							^super new initialize').
	self assert: (superclass theMetaClass parseTreeFor: #bar) = (RBParser parseMethod: 'bar
							self subclassResponsibility').
	self deny: (class directlyDefinesInstanceVariable: 'instVarName1').
	self deny: (class directlyDefinesInstanceVariable: 'instVarName2').
	self deny: (class directlyDefinesClassVariable: 'ClassVarName1').
	self deny: (class directlyDefinesClassVariable: 'ClassVarName2').
	self deny: (class theMetaClass directlyDefinesInstanceVariable: 'classInstVarName1').
	self deny: (class directlyDefinesMethod: #same).
	self deny: (class directlyDefinesMethod: #initialize).
	self deny: (class theMetaClass directlyDefinesMethod: #new).
	self assert: (class parseTreeFor: #different) = (RBParser parseMethod: 'different
							^instVarName1 + instVarName2').
	self assert: (class theMetaClass parseTreeFor: #bar) = (RBParser parseMethod: 'bar
							^self printString')