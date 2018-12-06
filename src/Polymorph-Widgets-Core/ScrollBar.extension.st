Extension { #name : #ScrollBar }

{ #category : #'*Polymorph-Widgets-Core' }
ScrollBar class >> imageColorFor: aScrollbar [
	"Return the (button) image colour for the given scrollbar."
	
	^self theme scrollbarImageColorFor: aScrollbar
]
