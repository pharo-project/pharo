Extension { #name : #Dictionary }

{ #category : #'*NewTools-Inspector-Extensions' }
Dictionary >> inspectorItems [
	<inspectorPresentationOrder: 0 title: 'Items'> 
	
	^ SpTablePresenter new 
		addColumn: (SpStringTableColumn 
			title: 'Key'
			evaluated: [ :each | GTObjectPrinter asTruncatedTextFrom: each key ]);
		addColumn: (SpStringTableColumn 
			title: 'Value' 
			evaluated: [ :each | GTObjectPrinter asTruncatedTextFrom: each value ]);
		items: self associations;
		yourself
]
