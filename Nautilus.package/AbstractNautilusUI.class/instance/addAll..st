build ui
addAll: aWindow
	| topHeight middleHeight bottomMorph bottomHeight delta |
	delta := 0.
	bottomHeight := 0.
	middleHeight := 0.
	topHeight := 0.
	sourceTextWidget := self buildCodePane.
	sourceTextWidget announcer when: RubTextAccepted send: #whenTextAcceptedInView: to: self.
	self buildTopPlugins
		ifNotNil:
			[ :top | 
			topHeight := top height.
			aWindow
				addMorph: top
				fullFrame:
					((0 @ 0 corner: 1 @ 0) asLayoutFrame bottomOffset: topHeight).
			delta := 4.
			topHeight := topHeight + delta.
			top color: Color transparent ].
	self buildTopBarOn: aWindow height: topHeight.
	topHeight := topHeight + self buttonsBarHeight + 4.
	self buildColumns: aWindow height: topHeight.
	self setShortcuts: #NautilusSourceCodeShortcuts to: aWindow.
	self buildMiddlePlugins
		ifNotNil:
			[ :middle | 
			middleHeight := middle height + 4.
			aWindow
				addMorph: middle
				fullFrame:
					((0 @ self navigationAreaProportion
						corner: 1 @ self navigationAreaProportion) asLayoutFrame
						bottomOffset: middleHeight).
			middle color: Color transparent ].
	sourceCodePanel := PanelMorph new.
	self setShortcuts: #NautilusSourceCodeShortcuts to: sourceCodePanel.
	sourceCodePanel changeProportionalLayout.
	sourceCodePanel
		addMorph: sourceTextWidget
		fullFrame: LayoutFrame identity.
	sourceCodePanel
		hResizing: #spaceFill;
		vResizing: #spaceFill.
	sourceCodeContainer := PanelMorph new.
	self
		setShortcuts: #NautilusSourceCodeShortcuts
		to: sourceCodeContainer.
	sourceCodeContainer
		changeTableLayout;
		listDirection: #leftToRight.
	sourceCodeContainer addMorph: sourceCodePanel.
	bottomMorph := self buildBottomPlugins.
	bottomMorph ifNotNil: [ bottomHeight := bottomMorph height + 8 ].
	aWindow
		addMorph: sourceCodeContainer
		fullFrame:
			((0 @ self navigationAreaProportion corner: 1 @ 1) asLayoutFrame
				topOffset: middleHeight;
				bottomOffset: bottomHeight negated).
	bottomMorph
		ifNotNil:
			[ 
			aWindow
				addMorph: bottomMorph
				fullFrame:
					((0 @ 1 corner: 1 @ 1) asLayoutFrame topOffset: bottomHeight negated).
			bottomMorph color: Color transparent ]