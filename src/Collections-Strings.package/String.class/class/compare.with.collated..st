primitives
compare: string1 with: string2 collated: order
	"Return 1, 2 or 3, if string1 is <, =, or > string2, with the collating order of characters given by the order array."

	| len1 len2 c1 c2 |
	order == nil ifTrue: [
		len1 := string1 size.
		len2 := string2 size.
		1 to: (len1 min: len2) do:[:i |
			c1 := (string1 basicAt: i).
			c2 := (string2 basicAt: i).
			c1 = c2 ifFalse: [c1 < c2 ifTrue: [^ 1] ifFalse: [^ 3]].
		].
		len1 = len2 ifTrue: [^ 2].
		len1 < len2 ifTrue: [^ 1] ifFalse: [^ 3].
	].
	len1 := string1 size.
	len2 := string2 size.
	1 to: (len1 min: len2) do:[:i |
		c1 := (string1 basicAt: i).
		c2 := (string2 basicAt: i).
		c1 < 256 ifTrue: [c1 := order at: c1 + 1].
		c2 < 256 ifTrue: [c2 := order at: c2 + 1].
		c1 = c2 ifFalse:[c1 < c2 ifTrue: [^ 1] ifFalse: [^ 3]].
	].
	len1 = len2 ifTrue: [^ 2].
	len1 < len2 ifTrue: [^ 1] ifFalse: [^ 3].
