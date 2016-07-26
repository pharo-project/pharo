accessing
fallbackFont
	^ fallbackFont ifNil: [ fallbackFont := FixedFaceFont new errorFont fontSize: self height ]