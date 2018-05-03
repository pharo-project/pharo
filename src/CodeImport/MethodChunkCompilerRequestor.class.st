"
I am an Object used to interact with the OpalCompiler during compilation of a method chunk. I keep a reference to the original input so that error messages can be properly analyzed later-on.
"
Class {
	#name : #MethodChunkCompilerRequestor,
	#superclass : #Object,
	#instVars : [
		'methodChunk',
		'fileCompileRequestor',
		'interactive'
	],
	#category : #'CodeImport-Utilities'
}

{ #category : #accessing }
MethodChunkCompilerRequestor >> fileCompileRequestor [
	^ fileCompileRequestor
]

{ #category : #accessing }
MethodChunkCompilerRequestor >> fileCompileRequestor: anObject [
	fileCompileRequestor := anObject
]

{ #category : #initialization }
MethodChunkCompilerRequestor >> initialize [

	super initialize.

	fileCompileRequestor := nil.
	interactive := nil.
	methodChunk := nil.
]

{ #category : #accessing }
MethodChunkCompilerRequestor >> interactive [
	^ interactive
]

{ #category : #accessing }
MethodChunkCompilerRequestor >> interactive: anObject [
	interactive := anObject
]

{ #category : #accessing }
MethodChunkCompilerRequestor >> methodChunk [
	^ methodChunk
]

{ #category : #accessing }
MethodChunkCompilerRequestor >> methodChunk: anObject [
	methodChunk := anObject
]

{ #category : #'interactive error protocol' }
MethodChunkCompilerRequestor >> notify: message at: location in: code [
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

{ #category : #'interactive error protocol' }
MethodChunkCompilerRequestor >> text [
	^ methodChunk contents
]
