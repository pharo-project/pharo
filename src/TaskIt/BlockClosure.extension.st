Extension { #name : #BlockClosure }

{ #category : #'*TaskIt' }
BlockClosure >> asTask [

	^ TKTTask valuable: self
]

{ #category : #'*TaskIt' }
BlockClosure >> future [

	^ self asTask future
]

{ #category : #'*TaskIt' }
BlockClosure >> schedule [

	^ self asTask schedule
]
