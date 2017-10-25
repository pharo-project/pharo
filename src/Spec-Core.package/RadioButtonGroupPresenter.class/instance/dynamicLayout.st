specs
dynamicLayout
	|lay|
	lay := SpecLayout composed
		newColumn: [ :column | 
			widgets keysDo: [ :item | column newRow: [ :row | row add: item ] ].
			column newRow: [ :row |  ] ].
	self layout: lay.
	^ lay