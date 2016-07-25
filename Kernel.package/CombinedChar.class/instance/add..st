composition
add: char

	| dict elem |
	codes ifNil: 
			[ codes := Array with: char. 
			combined := char. 
			^ true].
	dict := Compositions at: combined charCode ifAbsent: [^ false].
	elem := dict at: char charCode ifAbsent: [^ false].
	codes := codes copyWith: char.
	combined := Character leadingChar: self base leadingChar code: elem.
	^ true
