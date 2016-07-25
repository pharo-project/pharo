double dispatch
syntaxLookaround: lookaroundNode
	"Double dispatch from the syntax tree. 
	Special link can handle lookarounds (look ahead, positive and negative)."
	| piece |
	piece := lookaroundNode piece dispatchTo: self.
	^ RxmLookahaed with: piece