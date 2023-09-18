"
I represent all possible ABI Types
"
Class {
	#name : #TFAbiTypes,
	#superclass : #SharedPool,
	#classVars : [
		'DARWIN_X86_64_GNUW64',
		'DARWIN_X86_64_UNIX64',
		'DARWIN_X86_64_WIN64',
		'DEFAULT_ABI',
		'UNIX_X86_32_FASTCALL',
		'UNIX_X86_32_MS_CDECL',
		'UNIX_X86_32_PASCAL',
		'UNIX_X86_32_REGISTER',
		'UNIX_X86_32_STDCALL',
		'UNIX_X86_32_SYSV',
		'UNIX_X86_32_THISCALL',
		'UNIX_X86_64_GNUW64',
		'UNIX_X86_64_UNIX64',
		'UNIX_X86_64_WIN64',
		'WIN_X86_32_FASTCALL',
		'WIN_X86_32_MS_CDECL',
		'WIN_X86_32_PASCAL',
		'WIN_X86_32_REGISTER',
		'WIN_X86_32_STDCALL',
		'WIN_X86_32_SYSV',
		'WIN_X86_32_THISCALL',
		'WIN_X86_64_GNUW64',
		'WIN_X86_64_WIN64'
	],
	#category : #'ThreadedFFI-Base'
}

{ #category : #'class initialization' }
TFAbiTypes class >> initialize [

	"The default ABI is marked with a nil. The primitive will use the correct ABI depending on the platform"
	DEFAULT_ABI := nil.

	WIN_X86_64_WIN64 := 1.
	WIN_X86_64_GNUW64 := 2.

	DARWIN_X86_64_UNIX64 := 2.
	DARWIN_X86_64_WIN64 := 3.
	DARWIN_X86_64_GNUW64 := 4.

	UNIX_X86_64_UNIX64 := 2.
	UNIX_X86_64_WIN64 := 3.
	UNIX_X86_64_GNUW64 := 4.


	WIN_X86_32_SYSV      := 1.
	WIN_X86_32_STDCALL   := 2.
	WIN_X86_32_THISCALL  := 3.
	WIN_X86_32_FASTCALL  := 4.
	WIN_X86_32_MS_CDECL  := 5.
	WIN_X86_32_PASCAL   := 6.
	WIN_X86_32_REGISTER  := 7.

	UNIX_X86_32_SYSV      := 1.
	UNIX_X86_32_THISCALL  := 3.
	UNIX_X86_32_FASTCALL  := 4.
	UNIX_X86_32_STDCALL   := 5.
	UNIX_X86_32_PASCAL    := 6.
	UNIX_X86_32_REGISTER  := 7.
	UNIX_X86_32_MS_CDECL  := 8
]
