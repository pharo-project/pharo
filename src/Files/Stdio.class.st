"
I am a facade class to access standard output streams:

- standard input
- standard output
- standard error

I initialize standard streams in a lazy fashion, asking to the VM for the #stdioHandles. I cache those standard streams and release them on shutdown.

Known Bugs / subtleties
=======================

Windows VMs do now support the stdioHandles primitive if they are not compiled specially as command line binaries. To overcome that, on windows this class will create a normal file for std output / input / error.
"
Class {
	#name : #Stdio,
	#superclass : #Object,
	#instVars : [
		'Stderr',
		'Stdin',
		'Stdout'
	],
	#classVars : [
		'DefaultInstance'
	],
	#category : #'Files-Core'
}

{ #category : #'system startup' }
Stdio class >> cleanStdioHandles [
	^ self default cleanStdioHandles
]

{ #category : #accessing }
Stdio class >> default [
	^ DefaultInstance ifNil: [ DefaultInstance := self new ]
]

{ #category : #'initialize-release' }
Stdio class >> initialize [
	SessionManager default
		registerSystemClassNamed: self name
]

{ #category : #accessing }
Stdio class >> reset [
	DefaultInstance := nil
]

{ #category : #'system startup' }
Stdio class >> startUp: resuming [
	resuming ifTrue: [ self cleanStdioHandles ].
]

{ #category : #stdio }
Stdio class >> stderr [
	^ self default stderr

]

{ #category : #stdio }
Stdio class >> stdin [
	^ self default stdin
]

{ #category : #stdio }
Stdio class >> stdout [
	^ self default stdout

]

{ #category : #'system startup' }
Stdio >> cleanStdioHandles [

	Stderr := Stdin := Stdout := nil
]

{ #category : #private }
Stdio >> standardIOStreamNamed: moniker forWrite: forWrite [
	"Create if necessary and store default stdin, stdout and other files based on the their names"
	
	| handle |
	
	self flag: #todo. "This is an ugly hack, while waiting for a real fix for windows. There are several problems with this approach, but it allows us to run tests, etc."
	Smalltalk os isWin32 
		ifTrue: [ 
			[ ^ (File named: moniker asString) writeStream ]
			on: CannotDeleteFileException do: [
				"HACK: if the image is opened a second time windows barks about the already opened locked file"
				^ (File named: moniker asString, '_', (Random new nextInt: SmallInteger maxVal) asString) writeStream ]]. 	

	handle := self stdioHandles at: (#(stdin stdout stderr) identityIndexOf: moniker).
	handle ifNil: [ self error: moniker, ' is unavailable' ].
	^ StdioStream handle: handle file: (File named: moniker) forWrite: forWrite.
]

{ #category : #accessing }
Stdio >> stderr [

	^ Stderr ifNil: [ Stderr := self standardIOStreamNamed: #stderr forWrite: true ].
]

{ #category : #accessing }
Stdio >> stdin [

	^ Stdin ifNil: [ Stdin := self standardIOStreamNamed: #stdin forWrite: false ].
]

{ #category : #stdio }
Stdio >> stdioHandles [
	<primitive: 'primitiveFileStdioHandles' module: 'FilePlugin' error: ec>
	self primitiveFailed
]

{ #category : #accessing }
Stdio >> stdout [

	^ Stdout ifNil: [ Stdout := self standardIOStreamNamed: #stdout forWrite: true ].
]
