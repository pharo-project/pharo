instance creation
with: firstObject with: secondObject with: thirdObject with: fourthObject with: fifthObject
	"Answer a new instance of me, containing only the five arguments as
	elements."

	| newCollection |
	newCollection := self new: 5.
	newCollection at: 1 put: firstObject.
	newCollection at: 2 put: secondObject.
	newCollection at: 3 put: thirdObject.
	newCollection at: 4 put: fourthObject.
	newCollection at: 5 put: fifthObject.
	^newCollection