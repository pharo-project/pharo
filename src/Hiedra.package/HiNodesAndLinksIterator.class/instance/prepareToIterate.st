enumerating
prepareToIterate
	
	nodeByValue := Dictionary new.	
	values withIndexDo: [ :aValue :index |
		nodeByValue
			at: aValue
			put: (HiNode contents: aValue rowIndex: index) ].

	alreadyIteratedItems := Set new.
