Extension { #name : #Integer }

{ #category : #'*Kernel-Chronology' }
Integer >> asSeconds [
	^ Duration seconds: self
]
