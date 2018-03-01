Extension { #name : #BlockClosure }

{ #category : #'*Jobs' }
BlockClosure >> asJob [

	^ Job block: self.
]
