accessing
addPresentations: aCollection
	self notingPresentationChangeDo: [ 
		aCollection do: [ :each | self addPresentationSilently: each ] ]