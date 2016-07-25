accessing request
headerAddAll: keyedCollection
	"Add all key/value pairs of keyedCollection as headers.
	Note that #addAllMulti: is used"
	
	request headers 
		addAll: keyedCollection 