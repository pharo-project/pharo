tests
testEntriesDo
	| entries |
	1 to: 7 do: [ :each |
		store newEntry: (OmEntry content: each) ].

	entries := OrderedCollection new.
	store entriesDo: [ :entry | entries add: entry ].
	
	self assert: (entries collect: #content) equals: (1 to: 7) asOrderedCollection