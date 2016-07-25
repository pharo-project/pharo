copying
copyReplaceAll: oldSubCollection with: newCollection
	"Answer a copy of the receiver in which all occurrences of
	oldSubCollection have been replaced by newCollection "

	| aString startSearch currentIndex endIndex |
	
	aString := self.
	startSearch := 1.
	[(currentIndex := aString indexOfSubCollection: oldSubCollection startingAt: startSearch) > 0]
		whileTrue: [
			endIndex := currentIndex + oldSubCollection size - 1.
			aString := aString
					copyReplaceFrom: currentIndex
					to: endIndex
					with: newCollection.
				startSearch := currentIndex + newCollection size].

	^ aString