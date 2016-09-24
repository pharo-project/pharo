accessing
iconProvider
	" For 2.0/3.0 compatibility "
	self deprecated: 'Use iconNamed:'on: 'Pharo60'  in: '23 September 2016'.
	^ (Smalltalk respondsTo: #ui)
		ifTrue: [ (Smalltalk perform: #ui) icons]
		ifFalse: [ UITheme current ]