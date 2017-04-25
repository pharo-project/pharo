operations
get: urlObject
	"self get: 'http://stfx.eu/small.html'."
	
	^ self client
		url: urlObject;
		get;
		response
	