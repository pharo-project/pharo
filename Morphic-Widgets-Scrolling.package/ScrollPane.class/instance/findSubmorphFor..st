event handling
findSubmorphFor: ptY
	^ scroller
		findSubmorphBinary: [ :item | 
			(ptY between: item top and: item bottom)
				ifTrue: [ 0 ]
				ifFalse: [ 
					| itemCenter |
					itemCenter := (item top + item bottom) // 2.
					ptY < itemCenter
						ifTrue: [ -1 ]
						ifFalse: [ 1 ]	"found" ] ]