menu
menu: aMenu shifted: aBoolean
	(rule notNil and: [ criticsModel selectedItems notEmpty ])
		ifFalse: [ ^ aMenu ]. 
		
	self addNavigationMenuItemsTo: aMenu.
	aMenu addLine.
	self addMarkingMenuItemsTo: aMenu.
			
	criticsModel selectedItem
		ifNotNil: [ :critique |
		critique actions ifNotEmpty: [ :actions |
			aMenu addLine.
			actions do: [ :action |
				aMenu
					add: action description
					target: action
					selector: #actOnCritic:ofEntity:
					argumentList: {
						critique .
						critique sourceAnchor entity } ] ] ].
			
	^ aMenu