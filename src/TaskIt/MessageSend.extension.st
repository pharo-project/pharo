Extension { #name : #MessageSend }

{ #category : #'*TaskIt' }
MessageSend >> asTask [
	^ TKTTask valuable: self
]
