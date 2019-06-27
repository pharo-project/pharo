Class {
	#name : #SpTreeTableAdapterSingleColumnTest,
	#superclass : #SpAbstractTreeTableAdapterTest,
	#category : #'Spec2-Backend-Tests'
}

{ #category : #running }
SpTreeTableAdapterSingleColumnTest >> initializeTestedInstance [ 

	presenter
		addColumn: (SpStringTableColumn title: 'Value' evaluated: #printString);
		hideColumnHeaders;	
		roots: #(1 2 3);
		children: [ :aNumber | { aNumber * 2. aNumber * 3. aNumber * 10 } ].

]
