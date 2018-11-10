"
I'm an error that happens when credentials are bad.
"
Class {
	#name : #IceGitHubBadCredentialsError,
	#superclass : #IceGitHubError,
	#category : #'Iceberg-Plugin-GitHub-Model'
}

{ #category : #testing }
IceGitHubBadCredentialsError class >> isAppropriateFor: aResponse [ 
	^ aResponse status = 401
]

{ #category : #accessing }
IceGitHubBadCredentialsError >> messageBody [
	^ 'Credentials provided are wrong'
]
