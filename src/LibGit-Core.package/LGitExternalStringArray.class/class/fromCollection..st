instance creation
fromCollection: aCollection
	| instance |
	instance := self externalNew: aCollection size.
	aCollection withIndexDo: [ :element :index |
		instance at: index put: element ].
	
	^ instance