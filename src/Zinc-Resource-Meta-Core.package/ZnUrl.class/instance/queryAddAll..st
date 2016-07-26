accessing-query
queryAddAll: keyedCollection
	"Add all key/value pairs in keyedCollection as query parameters to the receiver.
	Note that we use #addAllMulti:"

	keyedCollection isEmpty
		ifFalse: [ 
			query ifNil: [ query := ZnMultiValueDictionary new ].
			query addAllMulti: keyedCollection ].
	^ keyedCollection