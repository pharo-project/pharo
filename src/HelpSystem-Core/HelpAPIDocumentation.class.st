"
This class represents the browsable package API help for the help system.

Instance Variables

"
Class {
	#name : #HelpAPIDocumentation,
	#superclass : #HelpOnHelp,
	#category : #'HelpSystem-Core-Help'
}

{ #category : #accessing }
HelpAPIDocumentation class >> bookName [
	^'API Documentation'
]

{ #category : #defaults }
HelpAPIDocumentation class >> builder [
	^PackageAPIHelpBuilder
]

{ #category : #accessing }
HelpAPIDocumentation class >> helpPackages [
	^#('HelpSystem-Core-Model' 'HelpSystem-Core-Utilities' 'HelpSystem-Core-UI')
]
