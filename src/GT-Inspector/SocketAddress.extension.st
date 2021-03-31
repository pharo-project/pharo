Extension { #name : #SocketAddress }

{ #category : #'*GT-Inspector' }
SocketAddress >> gtDisplayOn: stream [
	"Overwrite to get standard object behaviour"
	
	^ self printOn: stream
]
