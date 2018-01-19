"
I am NoStdio.

I am a Stdio that does not create files for windows but just streams stored in memory. 
This is an hack due to the missing handles in the VM for Windows.

I am used to set up an image to run an image without writing files on disk.

To install me, execute

  NoStdio install.

To get back to the normal situation, execute

  Stdio reset.
"
Class {
	#name : #NoStdio,
	#superclass : #Stdio,
	#category : #'Files-Core'
}

{ #category : #singleton }
NoStdio class >> install [
	DefaultInstance := self new
]

{ #category : #stdio }
NoStdio >> standardIOStreamNamed: moniker forWrite: forWrite [
	"Create if necessary and store default stdin, stdout and other files based on the their names"

	| handle |
	self flag: #todo.	"This is an ugly hack, while waiting for a real fix for windows. There are several problems with this approach, but it allows us to run tests, etc."
	Smalltalk os isWin32
		ifTrue: [ ^ [ (FileSystem memory / moniker asString) writeStream ]
				on: CannotDeleteFileException
				do: [ "HACK: if the image is opened a second time windows barks about the already opened locked file"
					^ (FileSystem memory / moniker asString , '_' , (Random new nextInt: SmallInteger maxVal) asString)
						writeStream ] ].
	handle := self stdioHandles at: (#(stdin stdout stderr) identityIndexOf: moniker).
	handle ifNil: [ self error: moniker , ' is unavailable' ].
	^ StdioStream handle: handle file: (File named: moniker) forWrite: forWrite
]
