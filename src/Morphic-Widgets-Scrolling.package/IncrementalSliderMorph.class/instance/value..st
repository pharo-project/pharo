accessing
value: aNumber
	"Set the slider value."

	(self sliderMorph ifNil: [^self]) scaledValue: aNumber.
	self model ifNotNil: [
		self setValueSelector ifNotNil: [
			self model perform: self setValueSelector with: self sliderMorph scaledValue]].
	self changed: #minEnabled; changed: #maxEnabled