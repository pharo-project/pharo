accessing
indexOfFirstUppercaseCharacter
	"Returns the index of the first Uppercase character.
	'uouFauhZ ' indexOfFirstUppercaseCharacter2 -> 4
	New implementation much faster than older one.
	"
	| size |
	size := self size.
	1 to: size do: [:i| 
		(self at: i) isUppercase 
			ifTrue: [^ i ]].
	^ 0