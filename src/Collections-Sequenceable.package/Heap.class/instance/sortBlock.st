accessing
sortBlock
	^ sortBlock ifNil: [ sortBlock := self defaultSortBlock ]