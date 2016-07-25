accessing
nextModel
	^ nextModel ifNil: [
		nextModel := context nextModel. 
		nextModel narrowWith: narrowString.
		nextModel ].