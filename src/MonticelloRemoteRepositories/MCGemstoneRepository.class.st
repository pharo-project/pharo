"
An explicit subclass of MCHttpRepository to provide a template for gemstone repositories.
"
Class {
	#name : #MCGemstoneRepository,
	#superclass : #MCHttpRepository,
	#category : 'MonticelloRemoteRepositories'
}

{ #category : #accessing }
MCGemstoneRepository class >> baseURL [
	^ 'http://ss3.gemtalksystems.com/ss/'
]

{ #category : #accessing }
MCGemstoneRepository class >> description [
	^ 'ss3.gemstone.com'
]

{ #category : #testing }
MCGemstoneRepository class >> isResponsibleFor: aUrl [
	^ (aUrl includesSubstring: 'ss3.gemstone.com/') or: [  aUrl includesSubstring: 'ss3.gemtalksystems.com/' ]
]

{ #category : #interface }
MCGemstoneRepository >> includesVersionNamed: aString [
	"directly do a filename check since squeaksource only stores mcz"
	^ self includesFileNamed: aString, '.mcz'
]
