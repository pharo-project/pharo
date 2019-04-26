Class {
	#name : #TreeTableAdapterSingleColumnTest,
	#superclass : #AbstractTreeTableAdapterTest,
	#category : #'Spec-BackendTests'
}

{ #category : #running }
TreeTableAdapterSingleColumnTest >> initializeTestedInstance [ 

	presenter
		addColumn: (StringTableColumn title: 'Value' evaluated: #printString);
		hideColumnHeaders;	
		roots: #(1 2 3);
		children: [ :aNumber | { aNumber * 2. aNumber * 3. aNumber * 10 } ].

]
