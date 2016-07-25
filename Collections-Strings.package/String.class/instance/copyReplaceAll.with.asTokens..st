copying
copyReplaceAll: oldSubstring with: newSubstring asTokens: ifTokens
	"Answer a copy of the receiver in which all occurrences of
	oldSubstring have been replaced by newSubstring.
	ifTokens (valid for Strings only) specifies that the characters
	surrounding the recplacement must not be alphanumeric.
		Bruce Simth,  must be incremented by 1 and not 
	newSubstring if ifTokens is true.  See example below. "

	| aString startSearch currentIndex endIndex |

	aString := self.
	startSearch := 1.
	[(currentIndex := aString indexOfSubCollection: oldSubstring startingAt: startSearch)
			 > 0]
		whileTrue: 
		[endIndex := currentIndex + oldSubstring size - 1.
		(ifTokens not
			or: [(currentIndex = 1
					or: [(aString at: currentIndex-1) isAlphaNumeric not])
				and: [endIndex = aString size
					or: [(aString at: endIndex+1) isAlphaNumeric not]]])
			ifTrue: [aString := aString
					copyReplaceFrom: currentIndex
					to: endIndex
					with: newSubstring.
				startSearch := currentIndex + newSubstring size]
			ifFalse: [
				ifTokens 
					ifTrue: [startSearch := currentIndex + 1]
					ifFalse: [startSearch := currentIndex + newSubstring size]]].
	^ aString

"Test case:
	'test te string' copyReplaceAll: 'te' with: 'longone' asTokens: true   "
