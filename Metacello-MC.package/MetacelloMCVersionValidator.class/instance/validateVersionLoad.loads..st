loading
validateVersionLoad: version loads: loadList
	| list |
	list := loadList asOrderedCollection.
	list isEmpty
		ifTrue: [ list add: 'default' ].
	(version groups includes: 'Tests')
		ifTrue: [ list add: 'Tests' ].
	version load: list