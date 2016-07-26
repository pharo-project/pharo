initialization
initializePresenter
	"Initialize widgets behavior"

	versionList
		whenSelectedItemChanged: [ :selection | 
			selection 
				ifNil: [ toolbar releaseEnabled: false ]
				ifNotNil: [ toolbar releaseEnabled: selection isDevelopment.
						versionList menu: [ :aMenu | self loadMenuFor: selection in: aMenu ] ].
			self refreshConfiguration.
			self update ];
		displayBlock: [ :item | VSVersionMorph withVersion: item ];
		yourself.

	allVersionsCheckBox
		whenChangedDo: [ :selector | | shouldUpdateList | 
							shouldUpdateList := self hasChanges
								ifTrue: [ self confirm: 'There are unsaved changes. Ok to loose these changes?' ]
								ifFalse: [ true ].
							shouldUpdateList ifTrue: [ self versionList: configuration versions ] ].
	
	repository acceptBlock: [ project repository: repository text ].
	
	repositoryAddButton action: [ self updateRepository ]