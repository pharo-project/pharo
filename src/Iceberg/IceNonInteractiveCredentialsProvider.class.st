"
A credentials provider that retrieves defaults or throws exceptions (but do not interacts with the user)
"
Class {
	#name : #IceNonInteractiveCredentialsProvider,
	#superclass : #IceCredentialsProvider,
	#category : #'Iceberg-Security'
}

{ #category : #private }
IceNonInteractiveCredentialsProvider >> obtainPlaintextCredentials: response [
	"Maybe I can read file here?"
	LGitNoCredentialsProvided signal
]

{ #category : #private }
IceNonInteractiveCredentialsProvider >> obtainSshCredentials: response [
	"Maybe I can read a file here?"
	LGitNoCredentialsProvided signal
]
