Class {
	#name : #RBAbstractInstanceVariableTest,
	#superclass : #RBRefactoringTest,
	#category : #'Refactoring-Tests-Core'
}

{ #category : #running }
RBAbstractInstanceVariableTest >> setUp [
	super setUp.
	model := self abstractVariableTestData.
]

{ #category : #tests }
RBAbstractInstanceVariableTest >> testAbstractInstanceVariable [
	| refactoring class |
	refactoring := RBAbstractInstanceVariableRefactoring 
		variable: 'class'
		class: RBTransformationRuleTestData.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBTransformationRuleTestData.
	self assert: (class parseTreeFor: #class1) equals: (self parseMethod: 'class1 ^class').
	self assert: (class parseTreeFor: #class:) equals: (self parseMethod: 'class: anObject
	class := anObject').
	self assert: (class parseTreeFor: #superSends) equals: (self parseMethod: 'superSends
	| rule |
	rule := RBParseTreeRewriter new.
	rule addSearch: ''super `@message: ``@args'' 
				-> (
					[:aNode | 
					(self class1 withAllSubclasses 
						detect: [:each | each includesSelector: aNode selector]
						ifNone: [nil]) isNil] 
							-> ''self `@message: ``@args'').
	self rewriteUsing: rule').
	self assert: (class parseTreeFor: #checkMethod:) equals: (self parseMethod: 'checkMethod: aSmalllintContext 
	self class: aSmalllintContext selectedClass.
	(rewriteRule executeTree: aSmalllintContext parseTree) 
		ifTrue: 
			[(RecursiveSelfRule executeTree: rewriteRule tree initialAnswer: false) 
				ifFalse: 
					[builder 
						compile: rewriteRule tree printString
						in: self class1
						classified: aSmalllintContext protocols]]')
]

{ #category : #tests }
RBAbstractInstanceVariableTest >> testAbstractWithAssignmentUsed [
	| refactoring class |
	class := model classNamed: #Foo.
	refactoring := RBAbstractInstanceVariableRefactoring 
				model: model
				variable: 'instVarName2'
				class: class.
	self executeRefactoring: refactoring.
	self assert: (class parseTreeFor: #foo) 
		equals: (self parseMethod: 'foo
						^self instVarName21: 3').
	self 
		assert: (class parseTreeFor: #instVarName2:) equals: (self 
						parseMethod: 'instVarName2: anObject
						self instVarName21: anObject').
	self 
		assert: (class parseTreeFor: #instVarName21:) equals: (self 
						parseMethod: 'instVarName21: anObject
						^instVarName2 := anObject').
	self assert: (class parseTreeFor: #instVarName2) 
				equals: (self parseMethod: 'instVarName2
						^instVarName2').
	self 
		assert: ((model classNamed: #Bar) parseTreeFor: #foo) equals: (self 
						parseMethod: 'foo
						instVarName1 := instVarName1 + self instVarName2 + ClassVarName1')
]

{ #category : #tests }
RBAbstractInstanceVariableTest >> testAbstractWithDefaultNamesUsed [
	| refactoring class |
	class := model classNamed: #Foo.
	refactoring := RBAbstractInstanceVariableRefactoring 
				model: model
				variable: 'instVarName1'
				class: class.
	self executeRefactoring: refactoring.
	self 
		assert: (class parseTreeFor: #bar) equals: (self 
						parseMethod: 'bar
						"Add one to instVarName1"

						self instVarName11: self instVarName11 + 1').
	self 
		assert: (class parseTreeFor: #instVarName11:) equals: (self 
						parseMethod: 'instVarName11: anObject
						instVarName1 := anObject').
	self assert: (class parseTreeFor: #instVarName11) 
			equals: (self parseMethod: 'instVarName11
						^instVarName1').
	self 
		assert: ((model classNamed: #Bar) parseTreeFor: #foo) equals: (self 
						parseMethod: 'foo
						self instVarName11: self instVarName11 + instVarName2 + ClassVarName1')
]

{ #category : #'failure tests' }
RBAbstractInstanceVariableTest >> testInheritedName [
	self shouldFail: (RBAbstractInstanceVariableRefactoring 
			variable: 'name'
			class: RBBasicLintRuleTestData)
]

{ #category : #tests }
RBAbstractInstanceVariableTest >> testMetaclassInstanceVariables [
	| refactoring class |
	class := model metaclassNamed: #Foo.
	class addInstanceVariable: 'foo'.
	class compile: 'zzz ^foo := foo + foo * 2' classified: #(#testing).
	refactoring := RBAbstractInstanceVariableRefactoring 
				model: model
				variable: 'foo'
				class: class.
	self executeRefactoring: refactoring.
	self assert: (class parseTreeFor: #foo1) 
		  equals: (self parseMethod: 'foo1
						^foo').
	self assert: (class parseTreeFor: #foo:) 
		  equals: (self parseMethod: 'foo: anObject
						^foo := anObject').
	self assert: (class parseTreeFor: #zzz) 
		  equals: (self parseMethod: 'zzz ^self foo: self foo1 + self foo1 * 2')
]

{ #category : #'failure tests' }
RBAbstractInstanceVariableTest >> testNonExistantName [
	self shouldFail: (RBAbstractInstanceVariableRefactoring 
			variable: 'foo'
			class: RBBasicLintRuleTestData)
]
