installing
installAsTextStyle
	"FontSetNewYork installAsTextStyle"
	| selectors |
	(TextSharedInformation includesKey: self fontName) ifTrue: 
		[ (self confirm: self fontName , ' is already defined in TextSharedInformation.
Do you want to replace that definition?') ifFalse: [ ^ self ] ].
	selectors := (self class selectors select: [ :s | s beginsWith: 'size' ]) asSortedCollection.
	TextSharedInformation 
		at: self fontName
		put: (TextStyle fontArray: (selectors collect: [ :each | self perform: each ]))