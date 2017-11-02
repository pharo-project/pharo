Extension { #name : #MCClassDefinition }

{ #category : #'*Komitter-UI' }
MCClassDefinition >> addOperation: aMCRemoval on: aKOClass [

	aKOClass addClassDefinition: aMCRemoval
]

{ #category : #'*Komitter-UI' }
MCClassDefinition >> koClass [
	self className ifNil: [ ^ nil ] ifNotNil: [ :cname | ^ KomitClass trackedClass: cname ]
]
