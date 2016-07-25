accessing
labelFromString: aString
	"Make an iconic label from aString"

	self labelGraphic: (StringMorph contents: aString) imageForm
