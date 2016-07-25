update
updateText
	"Update the text contents."

	self model ifNotNil: [
		self getTextSelector ifNotNil: [
			self contents: (self model perform: self getTextSelector)]]