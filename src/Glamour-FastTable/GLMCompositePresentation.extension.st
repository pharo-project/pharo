Extension { #name : #GLMCompositePresentation }

{ #category : #'*Glamour-FastTable' }
GLMCompositePresentation >> fastList [
	^ self custom: GLMFastListPresentation new
]

{ #category : #'*Glamour-FastTable' }
GLMCompositePresentation >> fastTable [
	^ self custom: GLMFastTablePresentation new
]

{ #category : #'*Glamour-FastTable' }
GLMCompositePresentation >> fastTree [
	^ self custom: GLMFastTreePresentation new
]

{ #category : #'*Glamour-FastTable' }
GLMCompositePresentation >> fastTreeTable [
	^ self custom: GLMFastTreeTablePresentation new
]
