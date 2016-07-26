instance creation
encompassing: listOfPoints 
	"A number of callers of encompass: should use this method."
	| topLeft bottomRight |
	topLeft := bottomRight := listOfPoints first.
	listOfPoints allButFirstDo: [ :p | 
		topLeft := topLeft min: p.
		bottomRight := bottomRight max: p ].
	^ topLeft corner: bottomRight