open/close
toggleClosable
	"Reinstate the close box. Go via theme to maintain box order."
	
	mustNotClose 
	ifTrue: [ self makeClosable ]
	ifFalse: [ self makeUnclosable ]