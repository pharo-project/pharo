canvas drawing-text
drawString: aString in: bounds font: aFontOrNil color: aColor
	| font |
	font := aFontOrNil ifNil: [ TextStyle defaultFont ].
	self canvasClipBy: (bounds intersect: self canvas clipRect) during:[
			self canvas pathTransform restoreAfter:[ 
			self canvas pathTransform translateBy: bounds origin.
			self canvas pathTransform translateX: 0 Y: font getPreciseAscent.
			self flag: #todo.	"scaled, so that FT-Plugin and Athens don't mix cached faces"
			self canvas pathTransform scaleBy: 1.0000001 @ 1.0000001.
			self canvas setPaint: aColor.
			self canvas setFont: font.
			self canvas drawString: aString from: 1 to: aString size ] ]