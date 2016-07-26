private - layout
updateColor
	"Update the color state."

	|col|
	self getColorSelector ifNotNil: [
		col := (self model perform: self getColorSelector) ifNil: [Color transparent].
		self setColor: col]