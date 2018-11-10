Class {
	#name : #LGitTransportCertificateCheckCallback,
	#superclass : #LGitCallback,
	#category : #'LibGit-Core-FFI-Callbacks'
}

{ #category : #'callback spec' }
LGitTransportCertificateCheckCallback class >> fnSpec [
	^ #(int (void *cert, int valid, String host, void *payload))
]
