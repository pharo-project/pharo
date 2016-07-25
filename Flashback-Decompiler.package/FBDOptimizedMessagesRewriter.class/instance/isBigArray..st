testing
isBigArray: msgNode
	^ (msgNode selector = #braceStream:) and: [ msgNode receiver isVariable and: [ msgNode receiver name = #Array ] ]