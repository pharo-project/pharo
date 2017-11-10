Extension { #name : #MCGitHubRepository }

{ #category : #'*Metacello-Platform' }
MCGitHubRepository class >> basicFromUrl: aZnUrl [ 
	^ self location: aZnUrl asString
]

{ #category : #'*Metacello-Platform' }
MCGitHubRepository class >> urlSchemes [
	^ #(github)
]
