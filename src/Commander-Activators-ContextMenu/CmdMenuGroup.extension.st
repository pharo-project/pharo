Extension { #name : #CmdMenuGroup }

{ #category : #'*Commander-Activators-ContextMenu' }
CmdMenuGroup >> buildContextMenu: aMenu [

	self isInlined 
		ifTrue: [ self inlineContextMenuItemsInto: aMenu ]
		ifFalse: [ self buildContextSubMenuIn: aMenu ]

]

{ #category : #'*Commander-Activators-ContextMenu' }
CmdMenuGroup >> buildContextSubMenuIn: aMenu [

	| submenu |
	submenu := UIManager default newMenuIn: aMenu defaultTarget for: aMenu defaultTarget.
	
	contents do: [ :each | each buildContextMenu: submenu ].
	
	submenu hasItems ifTrue: [ 
		aMenu add: self name icon: self icon subMenu: submenu.
		self description ifNotNil: [:d | aMenu lastItem setBalloonText: d] ]
]

{ #category : #'*Commander-Activators-ContextMenu' }
CmdMenuGroup >> inlineContextMenuItemsInto: aMenu [

	aMenu addLine.
	
	contents do: [ :each | each buildContextMenu: aMenu ].
	
	aMenu addLine.
]

{ #category : #'*Commander-Activators-ContextMenu' }
CmdMenuGroup >> registerContextMenuItemsWithBuilder: aBuilder [

	self isActive ifFalse: [ ^self ].
	
	self isInlined
		ifTrue: [ aBuilder items ifNotEmpty: [aBuilder withSeparatorAfter] ]
		ifFalse: [ self registerContextSubMenuWithBuilder: aBuilder].
		
	contents do: [ :each | 
		each registerContextMenuItemsWithBuilder: aBuilder ].
	
	self isInlined & aBuilder items notEmpty 
		ifTrue: [ aBuilder withSeparatorAfter ]
]

{ #category : #'*Commander-Activators-ContextMenu' }
CmdMenuGroup >> registerContextSubMenuWithBuilder: aBuilder [

	(aBuilder item: self name)
		order: self order;
		parent: parentGroup name;
		target: self;
		icon: self icon;
		help: self description
]
