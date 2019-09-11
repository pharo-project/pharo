Extension { #name : #Object }

{ #category : #'*Calypso-SystemQueries' }
Object >> matchesQueryBrowserFilter: aQueryBrowserFilter [

	^self calypsoEnvironmentType match: self forQueryBrowserFilter: aQueryBrowserFilter
]
