instance creation
radioButton
	"Answer a button pre-initialized with radiobutton images."

	| f |
	^ self new
		onImage: (f := self iconNamed: #radioButtonOnIcon);
		pressedImage: (self iconNamed: #radioButtonPressedIcon);
		offImage: (self iconNamed: #radioButtonOffIcon);
		extent: f extent + (2 @ 0);
		yourself