private
autoSelection
	| aString firstIndex interval string size |

	(self searchingString isEmpty or: [ self useRegEx] )
		ifTrue: [^ 0 to:0 ].
	
	self isSourceSymbol ifFalse: [^ 0 to: 0].
		
	aString := self sourceTextModel getString asLowercase.
	
	aString = self defaultExplanation asLowercase
		ifTrue: [ ^ 0 to: 0].
	
	string := self searchingString asLowercase.
	(string includes: $*)
		ifTrue: [| list lastIndex|
			list := string substrings: '*'.
			firstIndex := aString findString: list first.
			lastIndex := (aString findString: list last startingAt: firstIndex) + list last size - 1.
			interval := firstIndex to: lastIndex]
		ifFalse: [
			firstIndex := aString findString: string.
			size := string size.
			interval := firstIndex to: (firstIndex + size - 1)].
		
	^ firstIndex = 0 
		ifTrue: [ 0 to:0 ]
		ifFalse: [interval]