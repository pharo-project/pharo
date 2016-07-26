event handling
processEnter: anEvent
	self choiceMenus do: [:embeddedMenu | 
		embeddedMenu selectedItem ifNotNil: [:item |
			item invokeWithEvent: anEvent.
			^true ] ].
	^false