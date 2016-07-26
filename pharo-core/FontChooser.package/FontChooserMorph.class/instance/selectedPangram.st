accessing
selectedPangram
	^ selectedPangram ifNil: [ self pangrams first value ]