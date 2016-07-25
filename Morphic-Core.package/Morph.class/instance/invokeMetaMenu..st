meta-actions
invokeMetaMenu: evt
	| menu |	
	self world ifNil: [ ^self ].
	
	menu := self buildMetaMenu: evt.
	self addTitleForHaloMenu: menu.	
	menu popUpEvent: evt in: self world
