examples
presentationInput
	"
	
	self presentationInput.
	
	Thanks to Tudor Girba aka Doru
	Then Shift+Tab makes the editing area appear at the center of the screen.
	Useful to input explanations during a presentation 
	"

	KMRepository reset.
	KMRepository default
		setAsGlobalCategory:
			((KMCategory named: 'Presentation')
				addKeymapEntry:
					(KMKeymap
						shortcut: Character tab shift
						action: [ 
							RubEditingArea new
								beWrapped;
								width: World width / 3;
								updateTextWith: '';
								takeKeyboardFocus;
								backgroundColor: Color paleYellow;
								cellInset: 50;
								onAnnouncement: MorphLostFocus do: [ :ann | ann morph delete ];
								font: (LogicalFont familyName: 'Source Sans Pro' pointSize: 20);
								openCenteredInWorld ]))