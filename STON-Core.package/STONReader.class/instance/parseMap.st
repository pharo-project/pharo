parsing
parseMap
	| map |
	map := STON mapClass new.
	self storeReference: map.
	self parseMapDo: [ :key :value |
		map at: key put: value ].
	^ map