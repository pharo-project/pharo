announcements
handleMethodRemoved: anAnnouncement
	| item sel itm |
	
	self isDisplayed ifFalse: [ ^ self ].
	refreshingBlockHolder ifNil: [ ^ self ].
	self okToChange ifFalse: [ ^ self ].
	
	item := anAnnouncement method.	"Item is a compiled methed, where the list is populated with RGMethod"
	
	((item methodClass notNil) and:[item methodClass isObsolete not]) ifFalse:[ ^ self ].
	
	sel := listModel selectedIndex.
	itm := listModel selectedItem.
	
	(itm notNil and: [ itm methodClass = item methodClass and: [ itm selector = item selector ] ])
		ifTrue: [ textModel hasUnacceptedEdits: false ].
	
	self
		messages:
			(listModel listItems
				remove: item asFullRingDefinition ifAbsent: [ nil ];
				yourself).
	
	listModel setSelectedIndex: sel