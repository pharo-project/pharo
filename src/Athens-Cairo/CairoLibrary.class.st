"
I provide access to cairo library. 
Since paths and names can be tricky in different versions, I have methods to solve this in any platforms I want cairo to run.
"
Class {
	#name : #CairoLibrary,
	#superclass : #FFILibrary,
	#category : #'Athens-Cairo-Library'
}

{ #category : #'accessing platform' }
CairoLibrary >> macModuleName [
	^ 'libcairo.2.dylib'
]

{ #category : #'accessing platform' }
CairoLibrary >> unix32ModuleName [
	"On different flavors of linux the path to library may differ
	depending on OS distro or whether system is 32 or 64 bit.
	Packages such as snapcraft can also change the path.
	Look in a number of known locations, or LD_LIBRARY_PATH."

	(#('/usr/lib/i386-linux-gnu' '/usr/lib32' '/usr/lib'),
			((OSEnvironment current at: 'LD_LIBRARY_PATH' ifAbsent: [ '' ]) substrings: ':'))
		do: [ :path | 
			| libraryPath |
			libraryPath := path asFileReference / 'libcairo.so.2'.
			libraryPath exists
				ifTrue: [ ^ libraryPath fullName ] ].
	self error: 'Cannot locate cairo library. Please check if it installed on your system'
]

{ #category : #'accessing platform' }
CairoLibrary >> unix64ModuleName [
	"On different flavors of linux the path to library may differ
	depending on OS distro or whether system is 32 or 64 bit.
	Packages such as snapcraft can also change the path.
	Look in a number of known locations, or LD_LIBRARY_PATH."

	(#('/lib/x86_64-linux-gnu' '/usr/lib'),
			((OSEnvironment current at: 'LD_LIBRARY_PATH' ifAbsent: [ '' ]) substrings: ':'))
		do: [ :path | 
			| libraryPath |
			libraryPath := path asFileReference / 'libcairo.so.2'.
			libraryPath exists
				ifTrue: [ ^ libraryPath fullName ] ].
	self error: 'Cannot locate cairo library. Please check if it installed on your system'
]

{ #category : #'accessing platform' }
CairoLibrary >> unixModuleName [
	"Backward compatibility"
	^ self unix32ModuleName
		
]

{ #category : #'accessing platform' }
CairoLibrary >> win32ModuleName [ 
	^ 'libcairo-2.dll'
]
