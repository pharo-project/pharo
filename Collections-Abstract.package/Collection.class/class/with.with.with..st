instance creation
with: firstObject with: secondObject with: thirdObject 
	"Answer an instance of me containing the three arguments as elements."

	^ self new
		add: firstObject;
		add: secondObject;
		add: thirdObject;
		yourself