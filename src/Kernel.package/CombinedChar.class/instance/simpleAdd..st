composition
simpleAdd: char

	| dict elem |
	codes ifNil: 
		[codes := Array with: char. 
		combined := char. 
		^ true].
	dict := Compositions at: combined charCode ifAbsent: [^ false].
	elem := dict at: char charCode ifAbsent: [^ false].
	combined := Character leadingChar: self base leadingChar code: elem.
	codes at: 1 put: combined.
	^ true
