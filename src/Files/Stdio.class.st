"
I am a facade class to access standard output streams:

- standard input
- standard output
- standard error

I initialize standard streams in a lazy fashion, asking to the VM for the #stdioHandles. I cache those standard streams and release them on shutdown.

The possible scenarios, i.e. for each of stdin, stderr and stdout:

    the stream is attached to a terminal (default on posix platforms, PharoConsole.exe on Windows (see below for Pharo.exe))
    the stream is redirected to a normal file, e.g. pharo pharo.image > output.txt
    the stream is redirected to a pipe, e.g. pharo pharo.image | tee output.txt

To know exactly which kind of stream you are using, you can use File >> #fileDescriptorType: (args are 0 1 2 for the stdios).


Windows Subtleties
=====================

If launched as a desktop app (not from the console), Pharo.exe will not be linked to external streams because none is created. To overcome that, the default behavior of this class is to create a normal file for the three stdio. This can be modified to:
	not create a file (execute #useNullStreams)
	use a memory stream (execute #useMemoryStreams).
"
Class {
	#name : #Stdio,
	#superclass : #Object,
	#classVars : [
		'CreateWriteStreamBlock',
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

{ #category : #private }
Stdio class >> createStdioFileFor: moniker [
	^ [ self createWriteStreamBlock cull: moniker asString ]
		on: CannotDeleteFileException
		do:
			[ "HACK: if the image is opened a second time windows barks about the already opened locked file"
			[ self createWriteStreamBlock
				cull: moniker asString , '_' , (Random new nextInt: SmallInteger maxVal) asString ]
				on: FileException
				do:
					[ "Nothing, we do not have the rights to write and cannot suppose a dependency to the class MemoryFileSystemFile" ] ]
]

{ #category : #accessing }
Stdio class >> createWriteStreamBlock [

	^ CreateWriteStreamBlock
		ifNil: [ self useDefaultStreams.
			CreateWriteStreamBlock ]
]

{ #category : #accessing }
Stdio class >> createWriteStreamBlock: aBlock [
	"Can be changed to any block that returns a writeStream"

	CreateWriteStreamBlock := aBlock.
	self cleanStdioHandles
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
	([ File stdioDescriptorIsATTY not ]
		on: PrimitiveFailed
		do: [ :ex | "HACK kept for retrocompatibility" Smalltalk os isWin32 ])
		ifTrue: [ ^ self createStdioFileFor: moniker ].
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

{ #category : #stdio }
Stdio class >> useDefaultStreams [
	<script>
	self createWriteStreamBlock: [ :aName | (File named: aName asString) writeStream ]
]

{ #category : #stdio }
Stdio class >> useNullStreams [
	<script>
	self createWriteStreamBlock: [ NullStream new ]
]
