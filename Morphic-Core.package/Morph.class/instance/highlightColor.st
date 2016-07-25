accessing
highlightColor
	
	^ (self valueOfProperty: #highlightColor)
		ifNotNil:
			[:val | val ifNil: [self error: 'nil highlightColor']]
		ifNil:
			[owner ifNil: [self color] ifNotNil: [owner highlightColor]]