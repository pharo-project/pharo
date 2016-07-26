model access
setValue: newValue
	"Called internally for propagation to model"
	self value: newValue.
	setValueSelector ifNotNil: [
		^model perform: setValueSelector with: value
		].