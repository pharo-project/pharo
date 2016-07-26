private
insertSender: aContext
	"Insert aContext and its sender chain between me and my sender.  Return new callee of my original sender."

	| context |
	context := aContext bottomContext.
	context privSender: self sender.
	self privSender: aContext.
	^ context