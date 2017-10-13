initialization
initializeWidgets
	"configurations"
	configurationList := (self instantiate: FastTablePresenter)
		icons: [ :configuration | self iconForConfiguration: configuration ];
		displayBlock: [:item | item asString ]; 
		"iconMaxSize: 16@16;"
		handlesDoubleClick: true;
		on: PharoShortcuts current browseShortcut 
			do: [ configurationList selectedItem 
					ifNotNil: [ :selection | selection configurationClass browse ] ];
		doubleClickAction: [ configurationList selectedItem ifNotNil: [ :selection | 
				self browseConfiguration: selection ] ];
		yourself.
	
	"toolbar"
	toolbar := self instantiate: VersionnerToolBar.