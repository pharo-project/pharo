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
CairoLibrary >> macLibraryName [
	^ 'libcairo.2.dylib'
]

{ #category : #'accessing platform' }
CairoLibrary >> unix32LibraryName [
	"On different flavors of linux the path to library may differ
	depending on OS distro or whether system is 32 or 64 bit.
	Packages such as snapcraft can also change the path.
	Look in a number of known locations, or LD_LIBRARY_PATH."

	^ FFIUnix32LibraryFinder findLibrary: 'libcairo.so.2'
]

{ #category : #'accessing platform' }
CairoLibrary >> unix64LibraryName [
	"On different flavors of linux the path to library may differ
	depending on OS distro or whether system is 32 or 64 bit.
	Packages such as snapcraft can also change the path.
	Look in a number of known locations, or LD_LIBRARY_PATH."

	^ FFIUnix64LibraryFinder findLibrary: 'libcairo.so.2'
]

{ #category : #'accessing platform' }
CairoLibrary >> unixLibraryName [
	"Backward compatibility"
	^ self unix32LibraryName
		
]

{ #category : #'accessing platform' }
CairoLibrary >> win32LibraryName [ 
	^ 'libcairo-2.dll'
]
