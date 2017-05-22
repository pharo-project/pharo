instance creation
fromCollection: aCollection
	| instance |
	instance := self externalNew: aCollection size.
	aCollection withIndexDo: [ :commit :index |
		instance at: index put: commit ].
	
	^ instance