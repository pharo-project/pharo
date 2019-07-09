Class {
	#name : #RBCustomTransformationTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #testing }
RBCustomTransformationTest >> testTransform [

	| transformation newClassName class |
	newClassName := (self changeMock name, 'Temporary') asSymbol.
	transformation := RBCustomTransformation with: [ :aModel |
		"add class"
		aModel
			defineClass: ('<1p> subclass: #<2s>
				instanceVariableNames: ''''
				classVariableNames: ''''
				poolDictionaries: ''''
				category: <3p>' 
			expandMacrosWith: 'Object'
			with: newClassName
			with: self class category).
		
		"add a comment in the class description"
		(aModel classNamed: newClassName) comment: 'Deprecated!!! Use super class'.
		"add an instance variable"
		(aModel classNamed: newClassName) addInstanceVariable: 'asdf' ].

	transformation transform.
	self assert: transformation model changes changes size equals: 3.
	
	class := transformation model classNamed: (self changeMock name, 'Temporary') asSymbol.
	self assert: (class directlyDefinesInstanceVariable: 'asdf')
]
