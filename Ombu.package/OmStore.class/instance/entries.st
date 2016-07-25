accessing
entries
	"Answer all the entries the this store contains"

	| entries |
	entries := OrderedCollection new.

	self entriesDo: [ :entry | entries add: entry ].

	^ entries