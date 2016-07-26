accessing
resetValue
	| context |
	context := GLMTransmissionContext new.
	context addPort: self.
	self changeValueTo: nil in: context.