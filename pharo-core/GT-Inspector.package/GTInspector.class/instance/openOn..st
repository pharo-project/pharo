scripting opening
openOn: aPage
	| window |
	window := super openOn: aPage.
	window extent: self class preferredExtent.
	window when: WindowResizing do: [ 
		(window isMinimized or: [ window isMaximized ]) ifFalse: [ 
			"we only wnat to react to real resizing,
			not to minization or maximization"
			self class setPreferredExtentIfWanted: window extent ] ].
	^ window