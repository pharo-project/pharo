"
Native-Browser is a package adding to the system the possibility of opening a directory in the native browser of the OS. 

Currently working on:
- OSX (32/64)
- Linux (32/64)
- Windows (32/64)

Examples
------------
	See class side in #examples protocol
"
Class {
	#name : #NativeBrowserOpenVisitor,
	#superclass : #OSPlatformVisitor,
	#instVars : [
		'pathString'
	],
	#category : #'Native-Browser'
}

{ #category : #examples }
NativeBrowserOpenVisitor class >> example [
	<example>
	FileSystem workingDirectory openInOSFileBrowser
]

{ #category : #examples }
NativeBrowserOpenVisitor class >> example2 [
	<example>
	self openOn: FileLocator home
]

{ #category : #'instance creation' }
NativeBrowserOpenVisitor class >> openOn: aFileReference [
	"Opens an OS-specific file and directory browser on ==aFileReference==. If aFileReference is a file, opens the browser on its containing directory instead."

	| ref |
	(aFileReference isNil or: [ aFileReference exists not ]) ifTrue: [ ^ nil ].
	ref := aFileReference isFile ifTrue: [ aFileReference parent ] ifFalse: [ aFileReference ].
	(ref isNil or: [ ref exists not ]) ifTrue: [ ^ nil ].

	self new
		pathString: ref pathString;
		visit
]

{ #category : #accessing }
NativeBrowserOpenVisitor >> pathString: aString [
	pathString := aString
]

{ #category : #'private-ffi-calls' }
NativeBrowserOpenVisitor >> privShellExecute: lpOperation file: lpFile parameters: lpParameters directory: lpDirectory show: nShowCmd [
	^ self ffiCall: #( 
			FFIConstantHandle ShellExecuteA(
     				0,
     				char* lpOperation,
         			char* lpFile,
     				char* lpParameters,
     				char* lpDirectory,
        			int nShowCmd)) module: #shell32
]

{ #category : #visiting }
NativeBrowserOpenVisitor >> visitMacOS: aPlatform [
	LibC runCommand: ('open "{1}"' format: {pathString})
]

{ #category : #visiting }
NativeBrowserOpenVisitor >> visitUnix: aPlatform [
	LibC runCommand: ('xdg-open "{1}"' format: {pathString})
]

{ #category : #visiting }
NativeBrowserOpenVisitor >> visitWindows: aPlatform [
	self
		privShellExecute: 'explore'
		file: pathString
		parameters: ''
		directory: ''
		show: 5	"SW_SHOW"
]
