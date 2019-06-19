Extension { #name : #CmdCommand }

{ #category : #'*Commander-Activators-ContextMenu' }
CmdCommand >> fillContextMenu: aMenu using: aCommandActivator [
	| item |
	
	item := aMenu 
		add: aCommandActivator menuItemName
		target: aCommandActivator 
		selector: #executeCommand.
	
	aCommandActivator setUpShortcutTipForMenuItem: item.
	self setUpIconForMenuItem: item.
	self description ifNotNil: [:d | item setBalloonText: d]
]

{ #category : #'*Commander-Activators-ContextMenu' }
CmdCommand >> registerContextMenuItemsFor: aCommandItem withBuilder: aBuilder [
	| item itemOrder |
	itemOrder := aCommandItem order.
	aCommandItem parentGroup isRoot not & aCommandItem parentGroup isInlined ifTrue: [ 
		"When we are inlining we should try keep order according to the order of parent.
		So we just put item order as deimal addition to parent order"
		itemOrder := aCommandItem parentGroup order + (itemOrder / 10000000.0)].
	item := (aBuilder item: aCommandItem name)
		parent: aCommandItem actualParentGroup name;
		order: itemOrder;
		help: aCommandItem description;
		action: [aCommandItem executeCommand]. 
	
	aCommandItem setUpShortcutTipForMenuItem: item.
	self setUpIconForMenuItem: item
]

{ #category : #'*Commander-Activators-ContextMenu' }
CmdCommand >> setUpIconForMenuItem: aMenuItemMorph [

	self defaultMenuIcon ifNotNil: [:icon |
		aMenuItemMorph icon: icon ]
]

{ #category : #'*Commander-Activators-ContextMenu' }
CmdCommand >> setUpIconNamed: aString forMenuItem: aMenuItemMorph [

	aMenuItemMorph icon: (aMenuItemMorph iconNamed: aString)
]
