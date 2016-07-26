accessing
intercellSpacing 
	^ intercellSpacing ifNil: [ self class defaultIntercellSpacing ]