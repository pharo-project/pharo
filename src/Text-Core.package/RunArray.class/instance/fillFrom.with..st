private
fillFrom: aCollection with: aBlock
	"Evaluate aBlock with each of aCollection's elements as the argument.
	Collect the resulting values into self. Answer self."

	| newRuns newValues lastLength lastValue |
	newRuns := (Array new: aCollection size) writeStream.
	newValues := (Array new: aCollection size) writeStream.
	lastLength := 0.
	lastValue := Object new.
	aCollection do: [:each | 
		| value |
		value := aBlock value: each.
		lastValue = value
			ifTrue: [lastLength := lastLength + 1]
			ifFalse:
				[lastLength > 0
					ifTrue:
						[newRuns nextPut: lastLength.
						newValues nextPut: lastValue].
				lastLength := 1.
				lastValue := value]].
	lastLength > 0
		ifTrue:
			[newRuns nextPut: lastLength.
			newValues nextPut: lastValue].
	self setRuns: newRuns contents setValues: newValues contents