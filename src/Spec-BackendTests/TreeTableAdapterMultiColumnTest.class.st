Class {
	#name : #TreeTableAdapterMultiColumnTest,
	#superclass : #AbstractTreeTableAdapterTest,
	#category : #'Spec-BackendTests'
}

{ #category : #running }
TreeTableAdapterMultiColumnTest >> initializeTestedInstance [ 

	presenter
		addColumn: (StringTableColumn title: 'Value' evaluated: #printString);
		addColumn: (ImageTableColumn title: 'Icon' evaluated: [ :x | self iconNamed: #add ]);
		hideColumnHeaders;
		roots: #(1 2 3);
		children: [ :aNumber | { aNumber * 2. aNumber * 3. aNumber * 10 } ].

]

{ #category : #running }
TreeTableAdapterMultiColumnTest >> testAddingColumnUpdatesWidget [

	self presenter addColumn: (StringTableColumn title: 'Value' evaluated: #printString).
	
	self assert: self adapter columnCount equals: 3
]
