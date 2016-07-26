conversion
asString 
	^ String streamContents: [ :str | self asStringOn: str ]
	