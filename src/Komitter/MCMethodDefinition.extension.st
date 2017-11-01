Extension { #name : #MCMethodDefinition }

{ #category : #'*Komitter-UI' }
MCMethodDefinition >> addOperation: aMCAddition on: aKOClass [ 
	
	aKOClass addMethodDefinition: aMCAddition
]

{ #category : #'*Komitter-UI' }
MCMethodDefinition >> koClass [
	^ self className 
		ifNil: [ nil ] 
		ifNotNil: [ :cname |
			self isExtensionMethod 
				ifTrue: [ KomitClass trackedClass: cname forExtension: self  protocol ]
				ifFalse: [ KomitClass trackedClass: cname ] ]
]
