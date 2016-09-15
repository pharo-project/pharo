instance creation
checkBox
	"Answer a button pre-initialized with checkbox images."

	| f |
	^ self new
		onImage: (f := self iconNamed: #checkBoxOnIcon);
		pressedImage: (self iconNamed: #checkBoxPressedIcon);
		offImage: (self iconNamed: #checkBoxOffIcon);
		extent: f extent + (2 @ 0);
		yourself