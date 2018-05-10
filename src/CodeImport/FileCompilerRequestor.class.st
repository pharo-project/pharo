"
I am an Object used to interact with the OpalCompiler.
I keep a reference to the original input so that error messages can be properly analyzed later-on.
"
Class {
	#name : #FileCompilerRequestor,
	#superclass : #Object,
	#instVars : [
		'contents',
		'fileReference',
		'interactive'
	],
	#category : #'CodeImport-Utilities'
}

{ #category : #accessing }
FileCompilerRequestor >> contents [
	^ contents
]

{ #category : #accessing }
FileCompilerRequestor >> contents: anObject [
	contents := anObject
]

{ #category : #accessing }
FileCompilerRequestor >> failBlock [
	^ [ :exception | exception pass ]
]

{ #category : #accessing }
FileCompilerRequestor >> fileReference [
	^ fileReference
]

{ #category : #accessing }
FileCompilerRequestor >> fileReference: anObject [
	fileReference := anObject
]

{ #category : #initialization }
FileCompilerRequestor >> initialize [
	interactive := false.
]

{ #category : #testing }
FileCompilerRequestor >> interactive [
	^ interactive
]

{ #category : #accessing }
FileCompilerRequestor >> interactive: aBoolean [
	interactive := aBoolean
]

{ #category : #'interactive error protocol' }
FileCompilerRequestor >> notify: message at: location in: code [
	| newSource |
	self flag: #hack.
	"Should use the new OPalWarnings directly instead of recreating a SyntaxErrorNotification"
	newSource := SyntaxErrorNotification 
		inClass: STCommandLineHandler
		withCode: code
		doitFlag: false
		errorMessage: message
		location: location.
		
	"If the syntax error notification is resumed, then the source was corrected and we have to announce that parsing can restart."					
	ReparseAfterSourceEditing signalWithNewSource: newSource
]

{ #category : #accessing }
FileCompilerRequestor >> text [
	"compatibility method for the method installer"
	^ self contents
]
