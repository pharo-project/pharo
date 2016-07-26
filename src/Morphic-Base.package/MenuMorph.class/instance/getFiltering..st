filtering
getFiltering: matchString
	^ self valueOfProperty: #matchString ifAbsentPut: [ String new ]