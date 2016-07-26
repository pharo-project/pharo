arithmetic
rem: aNumber 
	"Remainder defined in terms of quo:. See super rem:.
	This is defined only to speed up case of large integers."

	<primitive: 20>
	 aNumber isInteger
		ifTrue:
			[| ng rem |
			ng := self negative == aNumber negative == false.
			rem := (self digitDiv: aNumber neg: ng) at: 2.
			^ rem normalize].
	^super rem: aNumber