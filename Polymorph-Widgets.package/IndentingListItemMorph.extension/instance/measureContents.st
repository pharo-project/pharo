*Polymorph-Widgets
measureContents
	"Round up in case fractional."

	| f iconWidth |
	f := self fontToUse.
	iconWidth := self hasIcon
				ifTrue: [self icon width + 2]
				ifFalse: [0].
	^ ((13 * indentLevel + 15 + iconWidth + (contents widthToDisplayInTree: self) max: self minimumWidth)
		@ ((contents heightToDisplayInTree: self) max: f height) + (self layoutInset * 2)) ceiling