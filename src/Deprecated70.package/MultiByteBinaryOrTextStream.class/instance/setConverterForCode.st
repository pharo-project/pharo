fileIn/out
setConverterForCode

	| current |
	current := converter saveStateOf: self.
	self position: 0.
	self binary.
	((self next: 3) =  #[239 187 191]) ifTrue: [
		self converter: UTF8TextConverter new
	] ifFalse: [
		self converter: MacRomanTextConverter new.
	].
	converter restoreStateOf: self with: current.
	self text.
