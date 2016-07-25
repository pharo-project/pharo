canvas drawing-text
drawString: aString from: firstIndex to: lastIndex in: bounds font: aFontOrNil color: aColor underline: underline underlineColor: uc strikethrough: strikethrough strikethroughColor: sc
	| font |
	font := aFontOrNil ifNil: [ TextStyle defaultFont ].
	self canvasClipBy: (bounds intersect: self canvas clipRect) during:[ 
			self canvas pathTransform restoreAfter:[ | advance |
			self canvas pathTransform translateBy: bounds origin.
			self canvas pathTransform translateX: 0 Y: font getPreciseAscent.
			self flag: #todo.	"scaled, so that FT-Plugin and Athens don't mix cached faces"
			self canvas pathTransform scaleBy: 1.0000001 @ 1.0000001.
			self canvas setPaint: aColor.
			self canvas setFont: font.
			advance := self canvas drawString: aString asString from: 1 to: aString size.
			underline ifTrue:[
				self canvas setPaint: uc.
				self canvas drawShape:(0@1 extent: advance x @1).
			]]].

