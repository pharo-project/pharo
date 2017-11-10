Extension { #name : #MCBitbucketRepository }

{ #category : #'*Metacello-Platform' }
MCBitbucketRepository class >> basicFromUrl: aZnUrl [ 
	^ self location: aZnUrl asString
]

{ #category : #'*Metacello-Platform' }
MCBitbucketRepository class >> urlSchemes [
	^ #(bitbucket)
]
