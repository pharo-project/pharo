options
serverUrl: urlObject
	"Set the explicit external server URL to urlObject. Defaults to nil.
	urlObject should be a ZnUrl or a String that parses correctly to one.
	See also #url."
	
	^ self optionAt: #serverUrl put: urlObject asZnUrl