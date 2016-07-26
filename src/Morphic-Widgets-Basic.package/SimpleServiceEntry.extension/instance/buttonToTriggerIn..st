*Morphic-Widgets-Basic
buttonToTriggerIn: aFileList
	"Answer a button that will trigger the receiver service in a file list"

	| aButton |
	aButton :=  PluggableButtonMorph
					on: self
					getState: nil
					action: #performServiceFor:.
	aButton 
		arguments: { aFileList }.

	aButton
		color: Color transparent;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		label: self buttonLabel;
		askBeforeChanging: true;
		onColor: Color white offColor: Color white.
		aButton setBalloonText: self description.

	^ aButton