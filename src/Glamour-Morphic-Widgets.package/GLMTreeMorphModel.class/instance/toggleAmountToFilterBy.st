accessing
toggleAmountToFilterBy
	self amountToFilterBy notNil ifTrue: 
		[ self amountToFilterBy isZero 
			ifTrue: [ amountToFilterBy := self glamourPresentation amountToShow ]
			ifFalse: [ amountToFilterBy := 0 ].
		self changed: #rootNodes ]