Extension { #name : #Character }

{ #category : #'*SUnit-Core' }
Character class >> specialCharacterNames [
	^ Dictionary newFrom: { 
		$+ -> 'plus'.
		$- -> 'minus'.
		$/ -> 'slash'.
		$\ -> 'backslash'.
		$* -> 'star'.
		$~ -> 'tilda'.
		$< -> 'less than'.
		$> -> 'greater-than'.
		$= -> 'equals-sign'.
		$@ -> 'at sign'.
		$, -> 'comma'.
		$% -> 'percent sign'.
		$| -> 'pipe'.
		$& -> 'ampersand'.
		$? -> 'question mark'.
		$! -> 'exclamation mark'.
		self centeredDot -> 'centered dot'.
		self divide -> 'divide'.
		self plusOrMinus -> 'plus-minus'.
		self times -> 'times' }
]
