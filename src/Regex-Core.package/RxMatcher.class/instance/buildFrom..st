accessing
buildFrom: aSyntaxTreeRoot
	"Private - Entry point of matcher build process."

	markerCount := 0.  "must go before #dispatchTo: !"
	matcher := aSyntaxTreeRoot dispatchTo: self.
	matcher terminateWith: RxmTerminator new