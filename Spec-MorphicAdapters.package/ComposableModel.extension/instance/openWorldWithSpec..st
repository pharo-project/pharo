*Spec-MorphicAdapters
openWorldWithSpec: aSpec
	"Build the widget using the spec name provided as argument and display it into the world"

	^ self openWorldWithSpecLayout: (self retrieveSpec: aSpec)