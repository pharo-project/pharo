private
interactWithSelection
	| cell |
	
	self hasSelection ifFalse: [ ^ self ].
	"this is a horrible hack, but well... that's how it is implemented :("
	cell := (self container exposedRows at: self selectedRowIndex) submorphs first. "the cell is there"
	cell submorphsDo: [ :each | each update: #interact ]