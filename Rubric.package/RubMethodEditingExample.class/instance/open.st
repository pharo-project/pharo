view
open
	| window bar editor |
	window := (StandardWindow labelled: 'Method editor with shout') model: self.
	editor := self newScrolledText.
	editor hResizing: #spaceFill; vResizing: #spaceFill.
	window addMorph: (window newColumn: { window newRow: { self classSelector. self metaSwitch. self methodSelector}. editor} )
		fullFrame: LayoutFrame identity.
	^ window openInWorld