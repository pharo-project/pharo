comparing
howManyMatch: string 
	"Count the number of characters that match up in self and aString."
	| count shorterLength |
	
	count := 0.
	shorterLength := self size min: string size.
	1 to: shorterLength do: [:index |
		(self at: index) = (string at: index )
			ifTrue: [ count := count + 1 ]].
	^  count 
	
	