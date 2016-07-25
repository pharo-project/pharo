accessing
url
	"Return the base external URL (a new ZnUrl instance) to access me.
	This defaults to #localUrl but can be set explicitely using the #serverUrl option.
	Missing elements from #serverUrl are merged in from #localUrl."

	^ self serverUrl 
		ifNil: [ self localUrl ] 
		ifNotNil: [ :serverUrl | serverUrl inContextOf: self localUrl ]