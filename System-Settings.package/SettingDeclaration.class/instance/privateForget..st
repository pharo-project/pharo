user interface
privateForget: aReceiver 
	"do not notify aReceiver anymore"
	self removeActionsWithReceiver: aReceiver forEvent: self privateChangeEvent