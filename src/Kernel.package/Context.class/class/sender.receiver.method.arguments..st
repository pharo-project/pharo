instance creation
sender: s receiver: r method: m arguments: args 
	"Answer an instance of me with attributes set to the arguments."

	^(self newForMethod: m) setSender: s receiver: r method: m arguments: args