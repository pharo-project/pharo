"
I am a an object representing a Windows platform. Use myself to access platform specific features.

Please keep me polymorphic to the the other platform objects.

To get the current platform, you can evaluate:

OSPlatform current.
"
Class {
	#name : #WinPlatform,
	#superclass : #OSPlatform,
	#category : #'System-Platforms-Windows'
}

{ #category : #testing }
WinPlatform class >> isActivePlatform [
	"Answer whether the receiver is the active platform"
	^ false
]

{ #category : #accessing }
WinPlatform >> defaultMaximumPathLength [
		"This method returns the default maximum path length for the getCurrentWorkingDirectory implementation. Windows default PATH_MAX is 260.  PATH_MAX is needed to avoid buffer overflow.  In case PATH_MAX is not sufficient the method currentWorkingDirectoryWithBufferSize: can be used to give desired buffer size."
	 ^260

]

{ #category : #accessing }
WinPlatform >> family [
	^#Windows
]

{ #category : #accessing }
WinPlatform >> getPwdViaFFI: buffer size: bufferSize [
	"This method calls the Standard C Library getcwd() function. The name of the argument (arg1) should fit decompiled version. This method is used in getting the current working directory. getcwd is preffered over pwd because getcwd takes care of re-initialization of environment variables, whereas pwd needs implict re- initialization.
	
	We use FFI-Kernel for this because this is required for bootstrapping, and we ensure minimal dependencies."
	^ (ExternalLibraryFunction 
			name: '_getcwd'
			module: 'msvcrt.dll'
			callType: 0
			returnType: ExternalType char asPointerType
			argumentTypes: {
				ExternalType char asPointerType.
				ExternalType long })
					invokeWith: buffer with: bufferSize.

]

{ #category : #testing }
WinPlatform >> isWindows [
	^ true
]

{ #category : #accessing }
WinPlatform >> keyForValue: aKeyValue [
	^KeyboardKey valueForWindowsPlatform: aKeyValue.
]

{ #category : #accessing }
WinPlatform >> lineEnding [

	^ String crlf
]

{ #category : #accessing }
WinPlatform >> menuShortcutString [
	^ 'ctrl'
]

{ #category : #accessing }
WinPlatform >> virtualKey: virtualKeyCode [
	"Win32Platform virtualKey: $C charCode"

	(virtualKeyCode between: 65 "$A charCode" and: 90 "$Z")
		ifFalse: [^nil].
	"#($a $b $c $d $e $f $g $h $i $j $k $l $m $n $o $p $q $r $s $t $u $v $w $x $y $z)"
	
	^(#($a nil $c $d nil $f $g nil nil nil nil $l $m $n nil $p nil nil $s nil nil $v nil $x nil $z)
		at: virtualKeyCode-64) ifNotNil: [:char | char charCode]
]
