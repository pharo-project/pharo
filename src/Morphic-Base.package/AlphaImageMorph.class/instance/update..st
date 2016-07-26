accessing
update: aSymbol
	"Update the image if changed."
	
	super update: aSymbol.
	aSymbol = self getImageSelector ifTrue: [
		self updateImage]