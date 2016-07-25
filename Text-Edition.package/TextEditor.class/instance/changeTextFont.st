menu messages
changeTextFont
	"Present a menu of available fonts, and if one is chosen, apply it to the current selection."
	
	| curFont newFont attr startIndex |
	startIndex := self startIndex.
	curFont := self text fontAt: startIndex.
	newFont := StrikeFont fromUser: curFont.
	newFont ifNil: [ ^self ].
	attr := TextFontReference toFont: newFont.
	self applyAttribute: attr