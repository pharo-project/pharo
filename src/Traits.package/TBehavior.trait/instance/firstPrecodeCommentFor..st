accessing method dictionary
firstPrecodeCommentFor:  selector
	"If there is a comment in the source code at the given selector that preceeds the body of the method, return it here, else return nil"

	| method |
	"Behavior firstPrecodeCommentFor: #firstPrecodeCommentFor:"
	(#(Comment Definition Hierarchy) includes: selector)
		ifTrue: [^ nil].
	method := self compiledMethodAt: selector asSymbol ifAbsent: [^ nil].
	^method ast firstPrecodeComment.
	