Extension { #name : #IceUrlProtocol }

{ #category : #'*Iceberg-Plugin-GitHub' }
IceUrlProtocol class >> githubRequestUrlKey [
	"Return the key used in github REST responses to get the URL with the correct protocol"
	self subclassResponsibility
]
