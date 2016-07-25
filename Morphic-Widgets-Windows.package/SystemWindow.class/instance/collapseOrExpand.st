resize/collapse
collapseOrExpand
	"Collapse or expand the window, depending on existing state.
	Use the taskbar if present, otherwise do as normal."
	
	| cf |
	self isTaskbarPresent ifTrue: [^self minimizeOrRestore].
	isCollapsed
		ifTrue: 
			["Expand -- restore panes to morphics structure"
			isCollapsed := false.
			self activate.  "Bring to frint first"
			collapsedFrame := self getBoundsWithFlex.
			collapseBox ifNotNil: [collapseBox setBalloonText: 'Collapse this window'].
			self setBoundsWithFlex: fullFrame.
			paneMorphs reverseDo: 
					[:m |  self addMorph: m unlock.
					self world startSteppingSubmorphsOf: m].
			self
				addPaneSplitters;
				addGripsIfWanted]
		ifFalse: 
			["Collapse -- remove panes from morphics structure"
			isCollapsed := true.
			fullFrame := self getBoundsWithFlex.
			"First save latest fullFrame"
			paneMorphs do: [:m | m delete; releaseCachedState].
			self removePaneSplitters.
			self removeGrips.
			cf := self getCollapsedFrame.
			collapsedFrame ifNil: [collapsedFrame := cf].
			self setBoundsWithFlex: cf.
			collapseBox ifNotNil: [collapseBox setBalloonText: 'expand this window'].
			expandBox ifNotNil: [expandBox setBalloonText: 'expand to full screen']].
	self layoutChanged.
	
	"This gets invoked only if the taskbar is not present, if it is, then minimizeOrRestore is invoked (cf beginning of this method)"
	self announce: 
		(WindowCollapsed new
			window: self;
			yourself).