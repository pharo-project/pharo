api
openDialogWithSpec: aSpec
	"Build the widget using the spec name provided as argument and display it into a window"

	^ self openDialogWithSpecLayout: (self retrieveSpec: aSpec)