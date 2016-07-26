protocol
updateValue
	"Update the value."
	
	self model ifNotNil: [:m |
		self getValueSelector ifNotNil: [:s |
			self scaledValue: (m perform: s)]]