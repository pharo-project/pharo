api
buildWithSpec: aSpec
	"Build the widget using the spec name provided as argument"
	
	^ self buildWithSpecLayout: (self retrieveSpec: aSpec)