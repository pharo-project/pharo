public access
compile
	| methodNode |
	methodNode := self compile: sourceStream in: class classified: category notifying: requestor ifFail: self compilationContext failBlock.
	^ methodNode generate: self compilationContext compiledMethodTrailer  