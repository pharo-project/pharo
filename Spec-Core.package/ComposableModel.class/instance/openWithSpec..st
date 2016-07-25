api
openWithSpec: aSpec
	"Build the widget using the spec name provided as argument and display it into a window"

	^ self openWithSpecLayout: (self retrieveSpec: aSpec)