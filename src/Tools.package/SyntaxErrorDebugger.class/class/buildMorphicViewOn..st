instance creation
buildMorphicViewOn: aSyntaxError
	"Answer an Morphic view on the given SyntaxError."

	| window |
	window := (SystemWindow labelled: 'Syntax Error: ' , aSyntaxError error)
		model: aSyntaxError.
	window
		addMorph:
			((PluggableListMorph
				on: aSyntaxError
				list: #list
				selected: #listIndex
				changeSelected: nil
				menu: #listMenu:) doubleClickSelector: #listDoubleClick)
		frame: (0 @ 0 corner: 1 @ 0.15).
	window
		addMorph:
			(RubScrolledTextMorph new
				on: aSyntaxError
				text: #contents
				accept: #contents:notifying:
				readSelection: #contentsSelection
				menu: #codePaneMenu:shifted:)
		frame: (0 @ 0.15 corner: 1 @ 1).
	aSyntaxError addDependant: window.
	^ window openInWorldExtent: 380 @ 220