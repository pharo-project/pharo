instance creation
with: firstObject with: secondObject with: thirdObject with: fourthObject 
	"Answer an instance of me, containing the four arguments as the elements."

	^ self new
		add: firstObject;
		add: secondObject;
		add: thirdObject;
		add: fourthObject;
		yourself