initialization
initializePresenter
	"Initialize widgets behavior"

	newButton action: [ self newConfiguration ].
	loadButton action: [ (VSLoadProjectCommand target: self for: self) execute ].
	openButton action: [ owner configurationList selectedItem ifNotNil: [ :selection | 
				owner browseConfiguration: selection ]  ].