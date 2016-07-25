instance creation
with: firstObject with: secondObject with: thirdObject with: fourthObject with: fifthObject with: sixthObject
	"Answer an instance of me, containing the six arguments as the elements."

	^ self new
		add: firstObject;
		add: secondObject;
		add: thirdObject;
		add: fourthObject;
		add: fifthObject;
		add: sixthObject;
		yourself