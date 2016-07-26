initialization
initializePresenter
	"Initialize widgets behavior"
	configurationList
		whenSelectedItemChanged: [ :selection | 
			selection
				ifNil: [ toolbar openButton enabled: false ]
				ifNotNil: [ toolbar openButton enabled: true.
						configurationList menu: [ :aMenu | self loadMenuFor: selection in: aMenu ] ] ].
			