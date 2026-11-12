"
Show all variable writers.

I am a command to show all variable writers (the methods which assigns new value to the variables).

To show writers I ask the browser to spawn new query browser.

Internal Representation and Key Implementation Points.

    Instance Variables
	browser:		<ClyBrowser>
"
Class {
	#name : 'ClyShowVariableWriterCommand',
	#superclass : 'SycVariableNavigationCommand',
	#category : 'Calypso-SystemTools-QueryBrowser-Commands-Queries',
	#package : 'Calypso-SystemTools-QueryBrowser',
	#tag : 'Commands-Queries'
}

{ #category : 'execution' }
ClyShowVariableWriterCommand >> executeRefactoring [

	(Smalltalk tools toolNamed: #messageList) browseWritersOfVariables: variables 
]

{ #category : 'accessing' }
ClyShowVariableWriterCommand >> fullBrowserOrder [

	^ 2001
]

{ #category : 'accessing' }
ClyShowVariableWriterCommand >> name [
	^'Writers'
]

{ #category : 'accessing' }
ClyShowVariableWriterCommand >> order [

	^ 1000
]
