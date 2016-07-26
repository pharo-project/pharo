private
resizeField
	| topLeft bottomRight |
	self flag: #TODO.	"Maybe it could be cool to let the user choose if the field need to be at the top or the bottom."
	topLeft := table bounds left @ (table bounds bottom - self fieldHeigh).
	bottomRight := (table bounds right - table verticalScrollBarWidth - (actionButton width + 5))
		@ table bottom.
	field bounds: (topLeft corner: bottomRight)