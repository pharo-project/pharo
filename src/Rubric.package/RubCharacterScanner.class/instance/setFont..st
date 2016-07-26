private
setFont: fontNumber
	"Set the font by number from the textStyle."

	self setActualFont: (textStyle fontAt: fontNumber)