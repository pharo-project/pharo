instance creation
checkBox
	"Answer a button pre-initialized with checkbox images."

	| f |
	^ self new
		onImage: (f := Smalltalk ui icons iconNamed: #checkBoxOnIcon);
		pressedImage: (Smalltalk ui icons iconNamed: #checkBoxPressedIcon);
		offImage: (Smalltalk ui icons iconNamed: #checkBoxOffIcon);
		extent: f extent + (2 @ 0);
		yourself