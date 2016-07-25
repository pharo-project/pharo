user interface
privateForget: aReceiver selector: aSelector 
	"Private"
	self
		removeActionsSatisfying: [:action | action receiver == aReceiver
				and: [action selector = aSelector]]
		forEvent: self privateChangeEvent