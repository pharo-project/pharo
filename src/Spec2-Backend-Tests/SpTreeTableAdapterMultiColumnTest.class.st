Class {
	#name : #SpTreeTableAdapterMultiColumnTest,
	#superclass : #SpAbstractTreeTableAdapterTest,
	#category : #'Spec2-Backend-Tests'
}

{ #category : #running }
SpTreeTableAdapterMultiColumnTest >> initializeTestedInstance [
	super initializeTestedInstance.
	presenter addColumn: (SpImageTableColumn title: 'Icon' evaluated: [ :x | self iconNamed: #add ])
]

{ #category : #running }
SpTreeTableAdapterMultiColumnTest >> testAddingColumnUpdatesWidget [

	self presenter addColumn: (SpStringTableColumn title: 'Value' evaluated: #printString).
	
	self assert: self adapter columnCount equals: 3
]
