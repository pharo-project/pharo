primitives
translate: aString from: start  to: stop  table: table
	"Trivial, non-primitive version"
	| char |
	start to: stop do: [:i |
		(char := aString basicAt: i) < 256 ifTrue: [
			aString at: i put: (table at: char+1)].
	].
