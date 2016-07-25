accessing
computedHeight
	^ computedHeight
		ifNil: [computedHeight := self computedHeightFromContents + self layoutInset asPoint y + self cellInset asPoint y]