initialize-release
initializeContents
	startSpan := TxStartSpan text: self.
	endSpan := TxEndSpan text: self.
	startSpan next: endSpan