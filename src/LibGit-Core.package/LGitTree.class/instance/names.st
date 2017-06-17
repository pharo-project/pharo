accessing
names
	<todo: 'decide what to do with this:
	- will not work for trees not referenced from head (e.g. in an older commit)
	- very slow for large repositories because every entry is visited'>
	| names |
	names := OrderedCollection new.
	self repository head object tree allEntriesDo: [ :name :entry :payload |
		entry repository: self repository.
		entry object = self ifTrue: [ names add: name ].
		0 ].
	
	^ names