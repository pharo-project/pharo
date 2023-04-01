Extension { #name : #String }

{ #category : #'*Kernel' }
String >> asProtocolOf: aClass [

	^ aClass organization ensureProtocolNamed: self
]
