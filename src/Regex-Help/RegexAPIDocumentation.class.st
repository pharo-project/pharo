"
This is a custom help book providing the API documentation for the regular expression framework.

"
Class {
	#name : #RegexAPIDocumentation,
	#superclass : #RegexHelp,
	#category : #'Regex-Help'
}

{ #category : #accessing }
RegexAPIDocumentation class >> bookName [
	^'API Documentation'
]

{ #category : #defaults }
RegexAPIDocumentation class >> builder [
	^PackageAPIHelpBuilder
]

{ #category : #accessing }
RegexAPIDocumentation class >> helpPackages [
	^#('Regex-Core' 'Regex-Core-Exceptions' 'Regex-Core-Tests')
]
