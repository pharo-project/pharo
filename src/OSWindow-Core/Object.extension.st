Extension { #name : #Object }

{ #category : #'*OSWindow-Core' }
Object >> inspectInExternalWindow [
	^ GTInspector new openInExternalWindowOn: self
]
