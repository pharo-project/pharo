file paths
getWindowsFontFolderPath
	"Answer the windows font folder path.
	This is obtained through the Windows API if FFI is present,
	otherwise it is a guess !"

	| externalLibraryClass externalTypeClass fun buff |
	externalLibraryClass := Smalltalk globals at: #ExternalLibraryFunction ifAbsent: [  ].
	externalTypeClass := Smalltalk globals at: #ExternalType ifAbsent: [  ].
	(externalLibraryClass isNil or: [ externalTypeClass isNil ])
		ifTrue: [ ^ self guessWindowsFontFolderPath ].
	fun := externalLibraryClass
		name: 'SHGetFolderPathA'
		module: 'shfolder.dll'
		callType: 1
		returnType: externalTypeClass long
		argumentTypes:
			{(externalTypeClass long).
			(externalTypeClass long).
			(externalTypeClass long).
			(externalTypeClass long).
			(externalTypeClass char asPointerType)}.
	buff := ByteArray new: 1024.
	[ 
	| r |
	r := fun
		invokeWith: 0
		with: 16r0014
		with: 0
		with: 0
		with: buff	"CSIDL_FONTS" ]
		on: Error
		do: [ :e | 
			"will get error if ffiplugin is missing"
			^ self guessWindowsFontFolderPath ].
	^ (buff copyFrom: 1 to: (buff indexOf: 0) - 1) asString