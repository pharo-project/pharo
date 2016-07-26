accessing
listMorph: anObject
	"Set the value of listMorph"

	listMorph := anObject.
	anObject 
			on: #keyStroke 
			send: #keyStroke: 
			to: self.