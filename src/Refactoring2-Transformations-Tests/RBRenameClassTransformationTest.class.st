Class {
	#name : #RBRenameClassTransformationTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #accessing }
RBRenameClassTransformationTest class >> defaultTimeLimit [
	^30 seconds
]

{ #category : #tests }
RBRenameClassTransformationTest >> testTransform [

	| transformation class |
	transformation := (RBRenameClassTransformation
							rename: 'RBDummyClassToRename' asSymbol
							to: 'RBNewDummyClassName' asSymbol)
							transform.
							
	self assert: transformation model changes changes size equals: 1.
	
	class := transformation model classNamed: 'RBNewDummyClassName' asSymbol.
	self assert: (class parseTreeFor: #method1)
		  equals: (self parseMethod: 'method1 ^ self method2').
	self deny: (transformation model includesClassNamed: 'RBDummyClassToRename' asSymbol).
				
	class := transformation model classNamed: 'RBDummySubclassOfClassToRename' asSymbol.
	self assert: class superclass 
		  equals: (transformation model classNamed: 'RBNewDummyClassName' asSymbol).
	self assert: (class parseTreeFor: #symbolReference) 
		  equals: (self parseMethod: 'symbolReference ^ #RBNewDummyClassName').
	self assert: (class parseTreeFor: #reference) 
		  equals: (self parseMethod: 'reference ^ RBNewDummyClassName new')
]
