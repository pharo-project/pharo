instance creation
radioButton
	"Answer a button pre-initialized with radiobutton images."

	| f |
	^ self new
		onImage: (f := Smalltalk ui icons iconNamed: #radioButtonOnIcon);
		pressedImage: (Smalltalk ui icons iconNamed: #radioButtonPressedIcon);
		offImage: (Smalltalk ui icons iconNamed: #radioButtonOffIcon);
		extent: f extent + (2 @ 0);
		yourself