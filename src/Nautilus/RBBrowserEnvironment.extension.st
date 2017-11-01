Extension { #name : #RBBrowserEnvironment }

{ #category : #'*Nautilus' }
RBBrowserEnvironment >> browse [

	^ Smalltalk tools browser openInEnvironment: self
]
