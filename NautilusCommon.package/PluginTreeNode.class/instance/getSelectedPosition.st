private
getSelectedPosition

	^ selectedPosition ifNil: [| it |
		it := self item ifNotNil: [ self item second ].
		selectedPosition := self getPositionsList indexOf: it ]