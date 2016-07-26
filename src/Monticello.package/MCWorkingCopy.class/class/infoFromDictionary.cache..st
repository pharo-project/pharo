private
infoFromDictionary: aDictionary cache: cache
	| id |
	id := (aDictionary at: #id) asString.
	^ cache at: id ifAbsentPut:
		[MCVersionInfo
			name: (aDictionary at: #name ifAbsent: [''])
			id: (UUID fromString: id)
			message: (aDictionary at: #message ifAbsent: [''])
			date: ([Date fromString: (aDictionary at: #date)] ifError: [nil])
			time: ([Time fromString: (aDictionary at: #time)] ifError: [nil])
			author: (aDictionary at: #author ifAbsent: [''])
			ancestors: (self ancestorsFromArray: (aDictionary at: #ancestors ifAbsent: []) cache: cache)
			stepChildren: (self ancestorsFromArray: (aDictionary at: #stepChildren ifAbsent: []) cache: cache)]