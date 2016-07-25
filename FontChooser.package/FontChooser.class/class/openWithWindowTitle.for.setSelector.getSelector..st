open
openWithWindowTitle: titleString for: anObject setSelector: setSelector getSelector: getSelector
	"FontChooser openWithWindowTitle: 'Choose the Menu Font' for: StandardFonts setSelector: #menuFont: getSelector: #menuFont"
	
	| instance windowMorph world |
	instance := self new.
	instance 
		title: titleString;
		target: anObject;
		setSelector: setSelector;
		getSelector: getSelector.
	world := self currentWorld.
	(windowMorph := FontChooserMorph withModel: instance)
		"position: self currentWorld primaryHand position;"
		position: ((World width-640)/2)@((World height-480)/2);
		extent: 640@480;
		open.
	^windowMorph
 