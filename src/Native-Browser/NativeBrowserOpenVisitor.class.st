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
	#name : 'NativeBrowserOpenVisitor',
	#superclass : 'OSPlatformVisitor',
	#instVars : [
		'pathString'
	],
	#category : 'Native-Browser-Base',
	#package : 'Native-Browser',
	#tag : 'Base'
}

{ #category : 'examples' }
NativeBrowserOpenVisitor class >> example [
	<example>
	FileSystem workingDirectory openInOSFileBrowser
]

{ #category : 'examples' }
NativeBrowserOpenVisitor class >> example2 [
	<example>
	self openOn: FileLocator home
]

{ #category : 'instance creation' }
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

{ #category : 'accessing' }
NativeBrowserOpenVisitor >> pathString: aString [
	pathString := aString
]

{ #category : 'private - ffi' }
NativeBrowserOpenVisitor >> privShellExecute: lpOperation file: lpFile parameters: lpParameters directory: lpDirectory show: nShowCmd [
	^ self ffiCall: #(
			FFIConstantHandle ShellExecuteW(
     				int 0, "Operation is not associated with a window"
     				Win32WideString lpOperation,
         			Win32WideString lpFile,
     				Win32WideString lpParameters,
     				Win32WideString lpDirectory,
        			int nShowCmd)) module: #shell32
]

{ #category : 'visiting' }
NativeBrowserOpenVisitor >> visitMacOS: aPlatform [
	LibC runCommand: ('open "{1}"' format: {pathString})
]

{ #category : 'visiting' }
NativeBrowserOpenVisitor >> visitUnix: aPlatform [
	LibC runCommand: ('xdg-open "{1}"' format: {pathString})
]

{ #category : 'visiting' }
NativeBrowserOpenVisitor >> visitWindows: aPlatform [

	self
		privShellExecute: 'explore' asWin32WideString
		file: pathString asWin32WideString
		parameters: '' asWin32WideString
		directory: '' asWin32WideString
		show: 5 "SW_SHOW"
]
