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
	#classVars : [
		'Stderr',
		'Stdin',
		'Stdout'
	],
	#category : #'Files-Core'
}

{ #category : #'system startup' }
Stdio class >> cleanStdioHandles [

	Stderr := Stdin := Stdout := nil
]

{ #category : #'initialize-release' }
Stdio class >> initialize [
	SessionManager default
		registerSystemClassNamed: self name
]

{ #category : #stdio }
Stdio class >> standardIOStreamNamed: moniker forWrite: forWrite [
	"Create if necessary and store default stdin, stdout and other files based on the their names"

	| handle |
	"HACK kept for retrocompatibility"
	([ File stdioDescriptorIsATTY not ]
		on: PrimitiveFailed
		do: [ :ex | Smalltalk os isWin32 ])
		ifTrue: [ [ ^ (File named: moniker asString) writeStream ]
				on: CannotDeleteFileException
				do: [ "HACK: if the image is opened a second time windows barks about the already opened locked file"
					[ ^ (File named: moniker asString , '_' , (Random new nextInt: SmallInteger maxVal) asString) writeStream ]
						on: FileException
						do: [ "Nothing, we do not have the rights to write" ] ] ].
	handle := File stdioHandles at: (#(stdin stdout stderr) identityIndexOf: moniker).
	handle ifNil: [ self error: moniker , ' is unavailable' ].
	^ StdioStream handle: handle file: (File named: moniker) forWrite: forWrite
]

{ #category : #'system startup' }
Stdio class >> startUp: resuming [

	resuming ifTrue: [ self cleanStdioHandles ].
]

{ #category : #accessing }
Stdio class >> stderr [
	^ Stderr ifNil: [ Stderr := self standardIOStreamNamed: #stderr forWrite: true ]
]

{ #category : #accessing }
Stdio class >> stdin [

	^ Stdin ifNil: [ Stdin := self standardIOStreamNamed: #stdin forWrite: false ].
]

{ #category : #accessing }
Stdio class >> stdout [

	^ Stdout ifNil: [ Stdout := self standardIOStreamNamed: #stdout forWrite: true ].
]
