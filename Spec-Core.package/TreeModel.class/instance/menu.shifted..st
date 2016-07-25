api
menu: aMenu shifted: aBoolean
	| menu |
	
	(self allowMenuOnNoItem not and: [ self selectedItems isEmpty ])
		ifTrue: [ ^ nil ].
		
	
	menuBlockHolder value ifNil: [ ^ nil ].
	menuBlockHolder value isBlock ifFalse: [ ^ menuBlockHolder value ].
	
	menu := MenuModel new.
	
	menuBlockHolder value cull: menu cull: aBoolean.
	
	^ menu