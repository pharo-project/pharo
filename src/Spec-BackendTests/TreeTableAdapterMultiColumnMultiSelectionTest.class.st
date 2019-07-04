Class {
	#name : #TreeTableAdapterMultiColumnMultiSelectionTest,
	#superclass : #TreeTableAdapterSingleColumnMultiSelectionTest,
	#category : #'Spec-BackendTests'
}

{ #category : #running }
TreeTableAdapterMultiColumnMultiSelectionTest >> initializeTestedInstance [ 

	presenter
		addColumn: (StringTableColumn title: 'Value' evaluated: #printString);
		addColumn: (ImageTableColumn title: 'Icon' evaluated: [ :x | self iconNamed: #add ]);
		hideColumnHeaders;	
		beMultipleSelection;
		roots: #(1 2 3);
		children: [ :aNumber | { aNumber * 2. aNumber * 3. aNumber * 10 } ].

]
