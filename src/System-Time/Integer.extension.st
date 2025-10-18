Extension { #name : 'Integer' }

{ #category : '*System-Time' }
Integer >> asSeconds [
	^ Duration seconds: self
]

{ #category : '*System-Time' }
Integer >> asYear [

 	^ Year year: self
]
