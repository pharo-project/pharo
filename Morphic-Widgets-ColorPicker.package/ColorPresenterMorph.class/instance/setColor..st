accessing
setColor: aColor
	"Update the colour of the labels."

	self labelMorph color: aColor.
	self solidLabelMorph color: aColor asNontranslucentColor