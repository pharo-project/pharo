accessing
measureContents
	"Round up in case fractional."
	
	| f |
	f := self fontToUse.
	^((((f widthOfString: contents) max: self minimumWidth)  @ f height) + (self layoutInset * 2)) ceiling