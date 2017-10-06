coloring
useStyle: newStyle during: aBlockClosure
	"Execute aBlockClosure with newStyle and set back the style to the old one after"

	| oldStyle |
	oldStyle := self style.
	self style: newStyle.
	aBlockClosure
		ensure: [ self style: oldStyle ]