Class {
	#name : #SpTreeTableAdapterMultiColumnTest,
	#superclass : #SpAbstractTreeTableAdapterTest,
	#category : #'Spec2-Backend-Tests'
}

{ #category : #running }
SpTreeTableAdapterMultiColumnTest >> initializeTestedInstance [ 

	presenter
		addColumn: (SpStringTableColumn title: 'Value' evaluated: #printString);
		addColumn: (SpImageTableColumn title: 'Icon' evaluated: [ :x | self iconNamed: #add ]);
		hideColumnHeaders;
		roots: #(1 2 3);
		children: [ :aNumber | { aNumber * 2. aNumber * 3. aNumber * 10 } ].

]

{ #category : #running }
SpTreeTableAdapterMultiColumnTest >> testAddingColumnUpdatesWidget [

	self presenter addColumn: (SpStringTableColumn title: 'Value' evaluated: #printString).
	
	self assert: self adapter columnCount equals: 3
]
