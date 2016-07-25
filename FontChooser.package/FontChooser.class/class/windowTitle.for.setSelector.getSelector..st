instance creation
windowTitle: titleString for: anObject setSelector: setSelector getSelector: getSelector
	
	| instance answer |
	instance := self new.
	instance 
		title: titleString;
		target: anObject;
		setSelector: setSelector;
		getSelector: getSelector.
	(answer := FontChooserMorph withModel: instance)
		position: self currentWorld primaryHand position;
		extent: 450@220;
		createWindow.
	^answer