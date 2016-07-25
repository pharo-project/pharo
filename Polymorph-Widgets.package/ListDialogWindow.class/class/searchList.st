accessing
searchList
	^ searchList ifNil: [ searchList := OrderedCollection new ]