Extension { #name : #GLMMorphicRenderer }

{ #category : #'*OSWindow-Core' }
GLMMorphicRenderer >> openInExternalWindow: aRenderable [
    window := GLMOSWindowMorphWorldRenderer render: aRenderable from: self.
    ^ window open
]
