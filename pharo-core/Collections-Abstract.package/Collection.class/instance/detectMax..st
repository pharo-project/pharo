enumerating
detectMax: aBlock
	"Evaluate aBlock with each of the receiver's elements as the argument. 
	Answer the element for which aBlock evaluates to the highest magnitude.
	If collection empty, return nil.  This method might also be called elect:."

	| maxElement maxValue |
	self do: [:each | | val | 
		maxValue == nil
			ifFalse: [
				(val := aBlock value: each) > maxValue ifTrue: [
					maxElement := each.
					maxValue := val]]
			ifTrue: ["first element"
				maxElement := each.
				maxValue := aBlock value: each].
				"Note that there is no way to get the first element that works 
				for all kinds of Collections.  Must test every one."].
	^ maxElement