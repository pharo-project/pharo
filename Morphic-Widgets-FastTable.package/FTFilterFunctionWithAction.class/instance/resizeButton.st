private
resizeButton
	| topLeft bottomRight |
	self flag: #TODO.	"Maybe it could be cool to let the user choose if the field need to be at the top or the bottom."
	bottomRight := (table bounds right - table verticalScrollBarWidth) @ table bottom.
	topLeft := (bottomRight x - actionButton width) @ (table bounds bottom - self fieldHeigh).
	actionButton bounds: (topLeft corner: bottomRight)