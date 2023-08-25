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
WindowsResolver >> getWindowsMusicFolderPath [
	"Answer the windows music folder path.
	This is obtained through the Windows API if FFI is present, otherwise it is a guess !"

	| tfExternalFunctionClass tfBasicType fun buff |
	tfExternalFunctionClass := Smalltalk globals at: #TFExternalFunction ifAbsent: [ ^ self guessWindowsMusicFolderPath ].
	tfBasicType := Smalltalk globals at: #TFBasicType.

	fun := tfExternalFunctionClass
		       name: 'SHGetFolderPathA'
		       moduleName: 'shfolder.dll'
		       parameterTypes: {
				       tfBasicType slong.
				       tfBasicType slong.
				       tfBasicType slong.
				       tfBasicType slong.
				       tfBasicType pointer }
		       returnType: tfBasicType slong.

	buff := ByteArray new: 1024.

	[ TFSameThreadRunner uniqueInstance invokeFunction: fun withArguments: { 0. 16r000d. 0. 0. buff } ]
		on: Error
		do: [ :e | "will get error if ffiplugin is missing" ^ self guessWindowsMusicFolderPath ].

	^ (buff copyFrom: 1 to: (buff indexOf: 0) - 1) asString
]

{ #category : #origins }
WindowsResolver >> getWindowsVideosFolderPath [
	"Answer the windows videos folder path.
	This is obtained through the Windows API if FFI is present, otherwise it is a guess !"

	| tfExternalFunctionClass tfBasicType fun buff |
	tfExternalFunctionClass := Smalltalk globals at: #TFExternalFunction ifAbsent: [ ^ self guessWindowsVideosFolderPath ].
	tfBasicType := Smalltalk globals at: #TFBasicType.

	fun := tfExternalFunctionClass
		       name: 'SHGetFolderPathA'
		       moduleName: 'shfolder.dll'
		       parameterTypes: {
				       tfBasicType slong.
				       tfBasicType slong.
				       tfBasicType slong.
				       tfBasicType slong.
				       tfBasicType pointer }
		       returnType: tfBasicType slong.

	buff := ByteArray new: 1024.

	[ TFSameThreadRunner uniqueInstance invokeFunction: fun withArguments: { 0. 16r000e. 0. 0. buff } ]
		on: Error
		do: [ :e | "will get error if ffiplugin is missing" ^ self guessWindowsVideosFolderPath ].

	^ (buff copyFrom: 1 to: (buff indexOf: 0) - 1) asString
]

{ #category : #origins }
WindowsResolver >> guessWindowsMusicFolderPath [
	"Guess the location of the Windows music folder"
	| possibilities |

	possibilities := Set new.
	'cdefghijklmnopqrstuvwxyz' do: [ : drive |
		#('Music' 'My Music') do:[:path | | d |
			(d := (FileLocator driveNamed: drive asSymbol) resolve: path) exists
				ifTrue: [ possibilities add: d ] ] ].
	possibilities := possibilities asSortedCollection: [:a :b | a entry creationTime >= b  entry creationTime].
	possibilities ifNotEmpty:[ ^ possibilities first fullName ].
	^ nil
]

{ #category : #origins }
WindowsResolver >> guessWindowsVideosFolderPath [
	"Guess the location of the Windows videos folder"

	| possibilities |

	possibilities := Set new.
	'cdefghijklmnopqrstuvwxyz' do: [ : drive |
		#('Videos' 'My Videos') do:[:path | | d |
			(d := (FileLocator driveNamed: drive asSymbol) resolve: path) exists
				ifTrue: [ possibilities add: d ] ] ].
	possibilities := possibilities asSortedCollection: [:a :b | a entry creationTime >= b  entry creationTime].
	possibilities ifNotEmpty:[ ^ possibilities first fullName ].
	^ nil
]

{ #category : #origins }
WindowsResolver >> home [
	"Value of the $USERPROFILE environment variable."

	^ self directoryFromEnvVariableNamed: 'USERPROFILE'
]

{ #category : #origins }
WindowsResolver >> music [
	"Use SHGetFolderPathA to get the folder path in Windows"

	^ self getWindowsMusicFolderPath
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
	"Use SHGetFolderPathA to get the folder path in Windows"

	^ self getWindowsVideosFolderPath
]
