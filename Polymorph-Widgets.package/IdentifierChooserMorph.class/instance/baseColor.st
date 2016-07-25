accessing
baseColor
	^ baseColor ifNil: [baseColor := self defaultBaseColor]