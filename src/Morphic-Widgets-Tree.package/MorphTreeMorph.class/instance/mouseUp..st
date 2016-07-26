events-processing
mouseUp: event 
	"Fixed up highlight problems."
	| nodeMorph wasHigh |
	
	self enabled ifFalse: [ ^self ].
	mouseOverAllowed := false.
	nodeMorph := self scrollerSubMorphFromPoint: event position.

	wasHigh := nodeMorph notNil 
		ifTrue: [nodeMorph highlightedForMouseDown] 
		ifFalse: [false].
		
	self allNodeMorphs 
		do: [:m | 
				m highlightedForMouseDown 
					ifTrue: [m highlightForMouseDown: false]].
	wasHigh 
		ifFalse: [
			nodeMorph 
				ifNotNil: [^self]].

	self listManager mouseUp: event on: nodeMorph.

	