methods
stringConcatenation
	| string |
	string := '' yourself.
	(1 to: 10) do: [:i | string := string , i printString].
	^string