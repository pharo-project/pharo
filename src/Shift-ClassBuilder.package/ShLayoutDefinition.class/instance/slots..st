accessing
slots: anObject
	slots := anObject collect: [ :e | e asSlot ].