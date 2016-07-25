private
addItemShowing: aString from: startNumber to: endNumber
	
	lock critical: [ | item items |
		item := SystemProgressItemMorph labeled: aString from: startNumber to: endNumber.
		items := self bars size.
		items < 10 ifTrue: [ 
			self addMorphBack: item.
			self recenter ].
		^ item ].