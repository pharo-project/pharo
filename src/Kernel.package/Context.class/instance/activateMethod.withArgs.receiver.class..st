controlling
activateMethod: newMethod withArgs: args receiver: rcvr class: class 
	"Answer a Context initialized with the arguments."

	^Context 
		sender: self
		receiver: rcvr
		method: newMethod
		arguments: args