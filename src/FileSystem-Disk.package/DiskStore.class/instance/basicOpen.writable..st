public
basicOpen: aPath writable: aBoolean
	| string encoded |
	string := self stringFromPath: aPath.
	encoded := Primitives encode: string.
	^ Primitives open: encoded writable: aBoolean