composition
combinesWith: char

	| dict |
	codes ifNil: [^false].
	dict := Compositions at: combined charCode ifAbsent: [^false].
	dict at: char charCode ifAbsent: [^false].
	^true
