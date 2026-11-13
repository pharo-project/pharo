"
Requests package to be used as editing method package.

I am a command to request package from the user which should be used as editing method package
"
Class {
	#name : 'ClyRequestMethodPackageForEditorCommand',
	#superclass : 'ClyMethodEditorCommand',
	#category : 'Calypso-SystemTools-Core-Editors-Methods',
	#package : 'Calypso-SystemTools-Core',
	#tag : 'Editors-Methods'
}

{ #category : 'testing' }
ClyRequestMethodPackageForEditorCommand class >> canBeExecutedInMethodEditor: aTool [

	^ true
]

{ #category : 'execution' }
ClyRequestMethodPackageForEditorCommand >> executeRefactoring [
	| packageEditor |
	packageEditor := methodEditor protocolAndPackageEditor.
	packageEditor requestChangeBy: [ packageEditor requestPackage]
]

{ #category : 'accessing' }
ClyRequestMethodPackageForEditorCommand >> name [
	^'Select method package'
]
