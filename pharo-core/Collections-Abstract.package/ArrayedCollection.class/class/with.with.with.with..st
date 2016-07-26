instance creation
with: firstObject with: secondObject with: thirdObject with: fourthObject 
	"Answer a new instance of me, containing only the three arguments as
	elements."

	| newCollection |
	newCollection := self new: 4.
	newCollection at: 1 put: firstObject.
	newCollection at: 2 put: secondObject.
	newCollection at: 3 put: thirdObject.
	newCollection at: 4 put: fourthObject.
	^newCollection