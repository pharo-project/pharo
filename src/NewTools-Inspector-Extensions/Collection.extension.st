Extension { #name : #Collection }

{ #category : #'*NewTools-Inspector-Extensions' }
Collection >> inspectorItems [
	<inspectorPresentationOrder: 0 title: 'Items'> 
	
	^ SpTablePresenter new 
		addColumn: ((SpIndexTableColumn title: 'Index')
			sortFunction: #yourself ascending);
		addColumn: ((SpStringTableColumn 
			title: 'Value' 
			evaluated: [ :each | GTObjectPrinter asTruncatedTextFrom: each ])
			sortFunction: #printString ascending);
		items: self asOrderedCollection;
		yourself
]
