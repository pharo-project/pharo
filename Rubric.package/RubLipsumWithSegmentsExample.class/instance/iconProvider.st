accessing
iconProvider
	" For 2.0/3.0 compatibility "
	^ (Smalltalk respondsTo: #ui)
		ifTrue: [ (Smalltalk perform: #ui) icons]
		ifFalse: [ UITheme current ]