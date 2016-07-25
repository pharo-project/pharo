accessing
summary
	| selInfo |
	selInfo := self selectedInfo.
	^ selInfo 
		ifNil: ['']
		ifNotNil: [selInfo summary]