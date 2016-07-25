resize/collapse
minimizeOrRestore
	"Collapse or expand the window, depending on existing state"
	
	|mc windowEvent |
	isCollapsed
		ifTrue: ["Expand -- restore panes to morphics structure"
			self theme settings animationSettings useAnimation 
				ifTrue: [self animateRestoreFromMinimized].
			isCollapsed := false.
			"Bring to front first"
			self
				setBoundsWithFlex: fullFrame;
				comeToFront;
				show.
			mc := self modalChild.
			paneMorphs reverseDo: [:m | 
				mc ifNil: [m unlock].
				self addMorph: m.
				self world startSteppingSubmorphsOf: m].
			self activate]
		ifFalse: ["Collapse -- remove panes from morphics structure"
			isCollapsed := true.
			fullFrame := self getBoundsWithFlex.
			"First save latest fullFrame"
			paneMorphs
				do: [:m | m delete; releaseCachedState].
			self
				setBoundsWithFlex: (-100 @ -100 extent: 2 @ 2); "place offscreen"
				hide.
			self theme settings animationSettings useAnimation  
				ifTrue: [self animateMinimize].
			self isActive ifTrue: [
				self world navigateVisibleWindowForward]].
	self layoutChanged.
	
	"This gets invoked only if the taskbar is present, see sender of minimizeOrRestore"
	windowEvent := 
		self isMinimized 
			ifTrue: [ WindowCollapsed new ]
			ifFalse: [ WindowExpanded new ].

	windowEvent window: self.
	self announce: windowEvent.