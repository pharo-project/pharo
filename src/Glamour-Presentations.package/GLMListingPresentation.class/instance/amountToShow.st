accessing
amountToShow
	^ (amountToShowBlock ifNil: [ [ self defaultAmountToShow ] ]) glamourValue: self entity