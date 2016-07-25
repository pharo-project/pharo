fill-styles
desktopImageFor: aWorld
	"Answer the desktop image for the given world.
	Look for an image file with the name of the theme.
	Answer nil for none available."

	|file|
	file := FileSystem disk workingDirectory / self class themeName , 'jpg'.
	^ file exists ifTrue: [
		ImageReadWriter formFromFileNamed: file fullName]