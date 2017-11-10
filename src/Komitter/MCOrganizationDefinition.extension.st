Extension { #name : #MCOrganizationDefinition }

{ #category : #'*Komitter-UI' }
MCOrganizationDefinition >> addOperation: aMCModification on: aKOClass [ 
	
	^ aKOClass addOrganizationDefinition: aMCModification
]

{ #category : #'*Komitter-UI' }
MCOrganizationDefinition >> koClass [
	self className ifNil: [ ^ nil ] ifNotNil: [ :cname | ^ KomitClass trackedClass: cname ]
]
