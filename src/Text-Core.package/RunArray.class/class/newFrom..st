instance creation
newFrom: aCollection 
	"Answer an instance of me containing the same elements as aCollection."

	| runs values lastRun lastValue |
	runs := (Array new: aCollection size // 2) writeStream.
	values := (Array new: aCollection size // 2) writeStream.
	lastRun := 0.
	lastValue := Object new.
	aCollection do: [:x | 
		lastValue = x
			ifTrue: [lastRun := lastRun + 1]
			ifFalse:
				[lastRun > 0
					ifTrue:
						[runs nextPut: lastRun.
						values nextPut: lastValue].
				lastRun := 1.
				lastValue := x]].
	lastRun > 0
		ifTrue:
			[runs nextPut: lastRun.
			values nextPut: lastValue].
	^self basicNew setRuns: runs contents setValues: values contents

"	RunArray newFrom: {1. 2. 2. 3}
	{1. $a. $a. 3} as: RunArray
	({1. $a. $a. 3} as: RunArray) values
"