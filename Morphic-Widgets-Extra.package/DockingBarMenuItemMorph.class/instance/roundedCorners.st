submorphs-accessing
roundedCorners
	"Return a list of those corners to round"
	owner isFloating
		ifTrue: [^ #(1 4 )].
	owner isAdheringToTop
		ifTrue: [^ #(1 4 )].
	owner isAdheringToBottom
		ifTrue: [^ #(2 3 )].
	owner isAdheringToLeft
		ifTrue: [^ #(1 2 )].
	owner isAdheringToRight
		ifTrue: [^ #(3 4 )].
	^ #(1 2 3 4 )