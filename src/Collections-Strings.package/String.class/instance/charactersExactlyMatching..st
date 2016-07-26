comparing
charactersExactlyMatching: aString
	"Do a character-by-character comparison between the receiver and aString.  Return the index of the final character that matched exactly."

	| count |
	count := self size min: aString size.
	1 to: count do: [:i | 
		(self at: i) = (aString at: i) ifFalse: [
			^ i - 1]].
	^ count