initialization
createBox
	"create a button with default to be used in the label area"
	"Transcript show: self paneColor asString;  
	cr."
	| box |
	box := IconicButton new.
	box color: Color transparent;
		 target: self;
		 useSquareCorners;
		 borderWidth: 0.

	^ box