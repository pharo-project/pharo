menu
browseCritic
	criticsModel selectedItem
		ifNotNil: [ :element |
			| entity |
			entity := element sourceAnchor entity.
			entity isBehavior 
				ifTrue: [ entity theNonMetaClass browse ]
				ifFalse: [ entity browse ] ] 