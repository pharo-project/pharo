menu items
windowMenuOn: aBuilder
	"To inject your custom items in a specific place, note that each item increases in order by 1. For example, if you wanted your item to be the second item, you could give it an #order: 1.5"

	<windowMenu>
	| closableLabel draggableLabel maximizeLabel |
	(aBuilder item: #Close)
		order: 1.0;
		action: [ aBuilder model closeBoxHit ];
		icon: (self iconNamed: #windowCloseForm);
		enabledBlock: [ aBuilder model allowedToClose ];
		withSeparatorAfter.
	(aBuilder item: #About)
		action: [ aBuilder model showAbout ];
		icon: (self iconNamed: #smallHelpIcon);
		withSeparatorAfter.
	(aBuilder item: #'Change title...')
		action: [ aBuilder model relabel ];
		withSeparatorAfter.
	(aBuilder item: #'Send to back')
		action: [ aBuilder model sendToBack ].
	(aBuilder item: #'Make next-to-topmost')
		action: [ aBuilder model makeSecondTopmost ];
		withSeparatorAfter.
	(aBuilder item: #'Create window group')
		action: [ aBuilder model createWindowGroup ];
		withSeparatorAfter.
	closableLabel := aBuilder model mustNotClose
		ifFalse: [ #'Make unclosable' ]
		ifTrue: [ #'Make closable' ].
	(aBuilder item: closableLabel)
		action: [ aBuilder model perform: closableLabel asCamelCase uncapitalized ].
	draggableLabel := aBuilder model isSticky
		ifTrue: [ #'Make draggable' ]
		ifFalse: [ #'Make undraggable' ].
	(aBuilder item: draggableLabel)
		action: [ aBuilder model toggleStickiness ];
		withSeparatorAfter.
	maximizeLabel := aBuilder model isMaximized
		ifTrue: [ #Restore ]
		ifFalse: [ #Maximize ].
	(aBuilder item: maximizeLabel)
		action: [ aBuilder model expandBoxHit ];
		icon: self icons windowMaximizeForm.
	aBuilder model isCollapsed
		ifFalse: [ (aBuilder item: #'Window color...')
				action: [ aBuilder model setWindowColor ] ]