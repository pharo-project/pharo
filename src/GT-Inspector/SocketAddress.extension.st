Extension { #name : #SocketAddress }

{ #category : #'*GT-Inspector' }
SocketAddress >> displayStringOn: stream [
	"Overwrite to get standard object behaviour"
	
	^ self printOn: stream
]

{ #category : #'*GT-Inspector' }
SocketAddress >> gtDisplayOn: stream [
	"Overwrite to get standard object behaviour"
	
	^ self printOn: stream
]
