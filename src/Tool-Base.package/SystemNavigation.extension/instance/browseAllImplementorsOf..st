*Tool-Base
browseAllImplementorsOf: selector
	"Create and schedule a message browser on each method that implements 
	the message whose selector is the argument, selector. For example,  
	Smalltalk browseAllImplementorsOf: #at:put:."

	"Create and schedule a senders browser for aSelector."	

	^ self
		browseMessageList: (self allImplementorsOf: selector)
		name: 'Implementors of ' , selector
		autoSelect: selector
		refreshingBlock: [:message | message selector = selector ].