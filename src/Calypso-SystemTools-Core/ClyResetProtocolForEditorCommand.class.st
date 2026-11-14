"
Unclasifies a method.
"
Class {
	#name : 'ClyResetProtocolForEditorCommand',
	#superclass : 'ClyMethodEditorCommand',
	#category : 'Calypso-SystemTools-Core-Editors-Methods',
	#package : 'Calypso-SystemTools-Core',
	#tag : 'Editors-Methods'
}

{ #category : 'execution' }
ClyResetProtocolForEditorCommand >> executeRefactoring [

	methodEditor protocolAndPackageEditor resetProtocolAndPackage
]

{ #category : 'accessing' }
ClyResetProtocolForEditorCommand >> name [

	^ 'Unclassify method'
]
