accessing
addAll: keyedCollection
	"Note that we use #addAllMulti:"

	keyedCollection isEmpty
		ifFalse: [ self headers addAllMulti: keyedCollection ].
	^ keyedCollection