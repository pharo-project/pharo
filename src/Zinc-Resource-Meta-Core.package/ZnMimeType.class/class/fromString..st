instance creation
fromString: aString 
	| main endOfSub sub parts parameters |
	main := aString copyUpTo: $/.
	endOfSub := aString indexOf: $;.
	endOfSub := endOfSub = 0 
		ifTrue: [ aString size ]
		ifFalse: [ endOfSub - 1 ].
	sub := aString copyFrom: main size + 2 to: endOfSub.
	endOfSub = aString size ifTrue: [ ^ self main: main sub: sub ].
	parts := (aString copyFrom: endOfSub + 1 to: aString size) substrings: ';'.
	parameters := Dictionary new.
	parts do: [ :each | 
		parameters
			at: (each copyUpTo: $=) trimBoth 
			put: (each copyAfter: $=) trimBoth ].
	^ self 
		main: main
		sub: sub
		parameters: parameters