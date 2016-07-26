accessing
getDescription
	^ (self selectedNode)
		ifNil: [self status contents ifEmpty: ['Hit return in text fields to accept the input' translated]]
		ifNotNil: [:sel | (Text string: sel item label attribute: TextEmphasis bold), String cr, sel item description]