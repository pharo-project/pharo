Extension { #name : #Model }

{ #category : #'*Tool-Base' }
Model >> containingWindow [
	"Answer the window that holds the receiver. The dependents technique is
	odious and may not be airtight, if multiple windows have the same
	model. "
	^ self dependents
		detect: [:d | (d isSystemWindow)
				and: [d model == self]]
		ifNone: []
]
