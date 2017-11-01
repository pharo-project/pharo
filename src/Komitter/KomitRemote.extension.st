Extension { #name : #KomitRemote }

{ #category : #'*Komitter-UI' }
KomitRemote >> icon [

	^ nil
]

{ #category : #'*Komitter-UI' }
KomitRemote >> komitOn: aKomitterUI [

	^ aKomitterUI newKomitFor: self
]

{ #category : #'*Komitter-UI' }
KomitRemote >> label [

	^ ''
]
