Extension { #name : #MCServerRegistry }

{ #category : #'*Monticello-Tests' }
MCServerRegistry >> removeCredentialsFor: aString [ 

	registry removeKey: aString.
]
