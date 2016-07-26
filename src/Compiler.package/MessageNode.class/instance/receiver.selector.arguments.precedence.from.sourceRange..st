initialize-release
receiver: rcvr selector: selName arguments: args precedence: p from: encoder sourceRange: range
	"Compile."
	encoder noteSourceRange: range forNode: self.
	^self
		receiver: rcvr
		selector: selName
		arguments: args
		precedence: p
		from: encoder