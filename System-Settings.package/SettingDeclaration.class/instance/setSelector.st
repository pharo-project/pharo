accessing
setSelector
	^ setSelector ifNil: [self getSelector ifNotNil: [:gs | (gs, ':') asSymbol]]