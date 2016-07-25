private
boundsForKeyText: aString font: aFont
	| ktp ktw b |
	
	ktp := self owner hasSubMenu
		ifTrue: [ self owner right - self owner subMenuMarker width ]
		ifFalse: [ self owner right ].
	ktp := ktp - (ktw := aFont widthOfString: aString).
	b := ktp @ ((self owner bounds top + self owner bounds bottom - aFont height) // 2) extent: ktw @ self owner height.
	^ b