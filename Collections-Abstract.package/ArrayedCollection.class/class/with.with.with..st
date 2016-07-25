instance creation
with: firstObject with: secondObject with: thirdObject 
	"Answer a new instance of me, containing only the three arguments as
	elements."

	| newCollection |
	newCollection := self new: 3.
	newCollection at: 1 put: firstObject.
	newCollection at: 2 put: secondObject.
	newCollection at: 3 put: thirdObject.
	^newCollection