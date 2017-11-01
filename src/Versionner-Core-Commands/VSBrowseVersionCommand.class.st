"
Open a browser on the selected version of a configuration.
The receiver is a aCompiledMethod.
"
Class {
	#name : #VSBrowseVersionCommand,
	#superclass : #VSVersionCommand,
	#category : 'Versionner-Core-Commands'
}

{ #category : #execute }
VSBrowseVersionCommand >> execute [
	"I expect that target is an MCVersion"
	| cm |
	cm := MetacelloToolBox compiledMethodForVersion: target.
	Smalltalk tools browser 
			openOnClass: cm methodClass
			selector:  cm selector
]

{ #category : #accessing }
VSBrowseVersionCommand >> title [
	^ 'browse version'
]
