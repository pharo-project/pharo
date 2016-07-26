debugger access
releaseTo: caller 
	"Remove information from the receiver and the contexts on its sender 
	chain up to caller in order to break circularities."

	| contex senderContext |
	contex := self.
	[ contex == nil or: [ contex == caller ]]
		whileFalse: [
			senderContext := contex sender.
			contex singleRelease.
			contex := senderContext ]