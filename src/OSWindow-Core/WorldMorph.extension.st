Extension { #name : #WorldMorph }

{ #category : #'*OSWindow-Core' }
WorldMorph >> checkNewWindowSize [
	| newExtent |
	newExtent := self osWindow extent.
	newExtent ~= self extent ifTrue: [
		self osWindowRenderer newExtent: newExtent.
		self extent: newExtent.
	]
]

{ #category : #'*OSWindow-Core' }
WorldMorph >> clipboardText [
	^ self osWindow clipboardText
]

{ #category : #'*OSWindow-Core' }
WorldMorph >> clipboardText: aString [
	^ self osWindow clipboardText: aString
]
