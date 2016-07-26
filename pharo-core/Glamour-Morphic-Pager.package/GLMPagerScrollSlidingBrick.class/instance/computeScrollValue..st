as yet unclassified
computeScrollValue: aNumber
	|newValue delta|
	
	delta := self owner width - self width.
	newValue := (aNumber max: 0) min: delta.
		
	^ ((delta = 0)
		ifTrue: [ 0 ]
		ifFalse: [ newValue / delta ])