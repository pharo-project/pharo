Class {
	#name : #RBMethodNodeTest,
	#superclass : #TestCase,
	#category : 'AST-Tests-Core'
}

{ #category : #tests }
RBMethodNodeTest >> testSelectorAndArgumentNames [
	self assert:
		(RBParser parseMethod: 'one ^ self') selectorAndArgumentNames =
			'one'.
	self assert:
		(RBParser parseMethod: 'one:   aOne ^ self') selectorAndArgumentNames =
			'one:   aOne'.
	self assert:
		(RBParser parseMethod: 'one:   aOne two:   aTwo ^ self') selectorAndArgumentNames =
			'one:   aOne two:   aTwo'.
	self assert:
		(RBParser parseMethod: '*   aOne') selectorAndArgumentNames =
			'*   aOne'.
	self assert:
		(RBParser parseMethod: '**   aOne') selectorAndArgumentNames =
			'**   aOne'.
]
