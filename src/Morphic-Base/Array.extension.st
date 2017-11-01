Extension { #name : #Array }

{ #category : #'*Morphic-Base' }
Array >> asLayoutFrame [
	 ^ LayoutFrame new fromArray: self
]
