Extension { #name : 'MCServerRegistry' }

{ #category : '*Monticello-Tests' }
MCServerRegistry >> removeCredentialsFor: aString [

	self registry removeKey: aString
]
