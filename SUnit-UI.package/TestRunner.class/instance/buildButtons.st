building
buildButtons
	^ PanelMorph new
		changeTableLayout;
		listDirection: #leftToRight;
		layoutInset: 0;
		color: Color transparent;
		addAllMorphs: (self buttons collect: [ :each |
			PluggableButtonMorph new
				on: self
				getState: nil
				action: each second
				label: nil
				menu: nil;
				label: each first;
				setBalloonText: each third;
				getEnabledSelector: (each at: 4);
				hResizing: #spaceFill;
				yourself ]);
		yourself.