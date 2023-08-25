"
I am an expert on Windows filesystem conventions. I resolve origins according to these conventions.
"
Class {
	#name : #WindowsResolver,
	#superclass : #PlatformResolver,
	#category : #'FileSystem-Core-Base-Resolver'
}

{ #category : #accessing }
WindowsResolver class >> platformName [
	^ 'Win32'
]

{ #category : #origins }
WindowsResolver >> cache [
	"Because Windows does not define any location for putting cache files, we chose to use the preference directory. http://libertyboy.free.fr/computing/reference/envariables/"
	^ self preferences
]

{ #category : #origins }
WindowsResolver >> home [
	"Value of the $USERPROFILE environment variable."

	^ self directoryFromEnvVariableNamed: 'USERPROFILE'
]

{ #category : #origins }
WindowsResolver >> music [
	"Value of the $CSIDL_MYMUSIC environment variable."

	^ self directoryFromEnvVariableNamed: 'CSIDL_MYMUSIC'
]

{ #category : #origins }
WindowsResolver >> preferences [
	^ self directoryFromEnvVariableNamed: 'APPDATA' or: [ self home ]
]

{ #category : #origins }
WindowsResolver >> temp [

	^ self resolveString: Smalltalk os getTempPath
]

{ #category : #origins }
WindowsResolver >> videos [
	"Value of the $CSIDL_COMMON_VIDEO environment variable."

	^ self directoryFromEnvVariableNamed: 'CSIDL_COMMON_VIDEO'
]
