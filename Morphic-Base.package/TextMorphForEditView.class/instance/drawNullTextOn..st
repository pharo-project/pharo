drawing
drawNullTextOn: aCanvas
	"Just run the normal code to show selection in a window"
	aCanvas paragraph: self paragraph bounds: bounds color: color
