"
Show all variable references.

To show references I ask the browser to spawn new query browser.

Internal Representation and Key Implementation Points.

    Instance Variables
	browser:		<ClyBrowserMorph>
"
Class {
	#name : 'ClyShowVariableRefCommand',
	#superclass : 'SycVariableNavigationCommand',
	#category : 'Calypso-SystemTools-QueryBrowser-Commands-Queries',
	#package : 'Calypso-SystemTools-QueryBrowser',
	#tag : 'Commands-Queries'
}

{ #category : 'execution' }
ClyShowVariableRefCommand >> executeRefactoring [

	(Smalltalk tools toolNamed: #messageList) browseReferencesToVariables: variables 
]

{ #category : 'accessing' }
ClyShowVariableRefCommand >> fullBrowserOrder [

	^ 2000
]

{ #category : 'accessing' }
ClyShowVariableRefCommand >> name [
	^'References'
]
