updating
updateLabel
	"Update the label."

	self model ifNotNil: [
		self getLabelSelector ifNotNil: [
			self label: (self model perform: self getLabelSelector)]]