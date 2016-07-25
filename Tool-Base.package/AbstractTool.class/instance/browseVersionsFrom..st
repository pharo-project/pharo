method
browseVersionsFrom: aMethod
	"Create and schedule a Versions Browser, showing all versions of the 
	currently selected message. Answer the browser or nil."

	aMethod selector
		ifNil: [ self inform: 'Sorry, only actual methods have retrievable versions.'. ^nil ]
		ifNotNil: [:selector |
			Smalltalk tools versionBrowser browseVersionsForMethod: aMethod ]
				