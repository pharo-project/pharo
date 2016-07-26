instance creation
with: firstObject with: secondObject with: thirdObject with: fourthObject with: fifthObject
	"Answer an instance of me, containing the five arguments as the elements."

	^ self new
		add: firstObject;
		add: secondObject;
		add: thirdObject;
		add: fourthObject;
		add: fifthObject;
		yourself