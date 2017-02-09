rendering
actOnPresentationToolbarUpdated: ann
	| dock |

	self setLabelOf: window for: ann presentation.
	dock := self renderToolbarForPresentation: ann presentation inMorph: window.
	dock hResizing: #shrinkWrap.
	window toolbarBox: dock.