Extension { #name : #ScrollBar }

{ #category : #'*Morphic-Widgets-FastTable' }
ScrollBar >> canScrollDown [

	^value < 1
]

{ #category : #'*Morphic-Widgets-FastTable' }
ScrollBar >> canScrollUp [

	^value > 0
]

{ #category : #'*Morphic-Widgets-FastTable' }
ScrollBar >> pageDelta [
	^ pageDelta
]

{ #category : #'*Morphic-Widgets-FastTable' }
ScrollBar >> scrollDownByPageDelta [
	self scrollDown: self pageDelta
]

{ #category : #'*Morphic-Widgets-FastTable' }
ScrollBar >> scrollRestrictedDown: aValue [

	"only scroll if the scrollbar is not at its end yet"

	self canScrollDown ifFalse: [ ^self ].
	
	self scrollDown: aValue
]

{ #category : #'*Morphic-Widgets-FastTable' }
ScrollBar >> scrollRestrictedUp: aValue [

	"only scroll if the scrollbar is not at its end yet"

	self canScrollUp ifFalse: [ ^self ].
	
	self scrollUp: aValue
]

{ #category : #'*Morphic-Widgets-FastTable' }
ScrollBar >> scrollUpByPageDelta [
	self scrollUp: self pageDelta
]
