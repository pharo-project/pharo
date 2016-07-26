instance creation
with: firstObject with: secondObject 
	"Answer an instance of me containing the two arguments as elements."

	^ self new
		add: firstObject;
		add: secondObject;
		yourself