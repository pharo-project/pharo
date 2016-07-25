accessing
basicSelectedColor: anObject
	"Set the value of selectedColor"

	selectedColor := anObject.
	
	self
		changed: #selectedColor;
		changed: #red;
		changed: #green;
		changed: #blue;
		changed: #hue;
		changed: #saturation;
		changed: #brightness;
		changed: #alpha.
		
	self announcer announce: (ColorChanged to: selectedColor).