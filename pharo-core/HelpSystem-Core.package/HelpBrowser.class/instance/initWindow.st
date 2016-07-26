ui
initWindow
	| toolbar dock contentMorph |
	window := (Smalltalk at: #StandardWindow) new.
	window model: self.
	window title: 'Help Browser'.
	toolbar := window
		newToolbar:
			{(window
				newButtonFor: self
				getState: nil
				action: #refresh
				arguments: nil
				getEnabled: nil
				labelForm: (Smalltalk ui icons iconNamed: #refreshIcon)
				help: 'Refresh' translated)}.
	dock := window newToolDockingBar.
	dock addMorphBack: toolbar.
	window
		addMorph: dock
		fullFrame: ((0 @ 0 corner: 1 @ 0) asLayoutFrame bottomOffset: dock minExtent y).	"Tree"
	treeMorph := PluggableTreeMorph new.
	treeMorph
		model: self;
		setSelectedSelector: #onItemClicked:.
	window
		addMorph: treeMorph
		fullFrame: ((0 @ 0 corner: 0.3 @ 1) asLayoutFrame topOffset: dock minExtent y).	"Text"
	contentMorph := self sourceTextModel newScrolledText
		beForSmalltalkScriptingWithDefaultFont;
		beWrapped;
		yourself.
	contentMorph menuProvider: self selector: #shoutMorphFillMenu:.
	window
		addMorph: contentMorph
		fullFrame: ((0.3 @ 0 corner: 1 @ 1) asLayoutFrame topOffset: dock minExtent y)