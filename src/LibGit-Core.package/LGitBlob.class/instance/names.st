accessing
names
	| names |
	names := OrderedCollection new.
	self repository head object tree allEntriesDo: [ :rootPath :entry :payload |
		entry repository: self repository.
		entry object = self ifTrue: [ names add: entry filename ].
		0 ].
	
	^ names