drawing-private
drawKeyTextOn: aCanvas 
	"Draw the key text on the canvas."
	
	self keyText ifNil: [^self].
	self itemShortcut drawOn: aCanvas