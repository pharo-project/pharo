Extension { #name : #Class }

{ #category : #'*NECompletion' }
Class >> commonSuperclass: aClass [

	^ self allSuperclasses detect: [ :class | (aClass allSuperclasses includes: class) ]

	
]
