actions
openRepository
	self repository ifNotNil: [:repos | repos morphicOpen: self workingCopy ]