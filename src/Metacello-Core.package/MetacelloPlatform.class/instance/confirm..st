user interaction
confirm: aString
	
	^(Smalltalk hasClassNamed: #UIManager)
		ifTrue: [ (Smalltalk classNamed: #UIManager) default perform: #confirm: with: aString ]
		ifFalse: [
			"throw warning and answer true, if no way to announce"
			Warning signal: aString.
			true ]