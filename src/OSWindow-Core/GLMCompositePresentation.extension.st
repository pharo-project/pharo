Extension { #name : #GLMCompositePresentation }

{ #category : #'*OSWindow-Core' }
GLMCompositePresentation >> openInExternalWindow [
    ^ self defaultRenderer openInExternalWindow: self
]

{ #category : #'*OSWindow-Core' }
GLMCompositePresentation >> openInExternalWindowOn: anObject [ 
    ^ self openOn: anObject inExternalWindowWith: self defaultRenderer
]

{ #category : #'*OSWindow-Core' }
GLMCompositePresentation >> openInExternalWindowWith: aRenderer [ 
    ^ aRenderer openInExternalWindow: self
]

{ #category : #'*OSWindow-Core' }
GLMCompositePresentation >> openOn: anObject inExternalWindowWith: aRenderer [ 
     
    self startOn: anObject. 
    ^ aRenderer openInExternalWindow: self
]
