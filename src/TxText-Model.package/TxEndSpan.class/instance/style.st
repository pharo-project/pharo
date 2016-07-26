accessing
style
	"My attributes are always same as my previous span,
	unless text is empty, then it is default text attributes"
	
	^ text isEmpty
		ifTrue: [ text defaultStyle ]
		ifFalse: [ prev style ]