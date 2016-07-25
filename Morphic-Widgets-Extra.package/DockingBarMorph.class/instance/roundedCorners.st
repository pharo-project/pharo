rounding
roundedCorners
	"Return a list of those corners to round"
	self isAdheringToTop
		ifTrue: [^ #(2 3 )].
	self isAdheringToBottom
		ifTrue: [^ #(1 4 )].
	self isAdheringToLeft 
		ifTrue: [^ #(3 4 )].
	self isAdheringToRight
		ifTrue: [^ #(1 2 )].
	^ #(1 2 3 4 )