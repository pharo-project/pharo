accessing request
queryAddAll: keyedCollection
	"Add all key -> value elements of keyedCollection 
	as HTTP query parameters to the current request.
	Note that #queryAt:add: is used to preserve duplicate keys."

	keyedCollection keysAndValuesDo: [ :key :value |
		self queryAt: key add: value ]