ast manipulation
wrapInContext: hook link: aLink
	^RFMessageNode 
		receiver: aLink asLiteralNode
		selector: #valueInContext:
		arguments: {self encloseInBlock: hook}
				

	