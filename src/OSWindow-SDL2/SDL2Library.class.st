"
I am the FFILibrary to look up and handle all FFI calls in SDL
"
Class {
	#name : 'SDL2Library',
	#superclass : 'FFILibrary',
	#category : 'OSWindow-SDL2-Bindings',
	#package : 'OSWindow-SDL2',
	#tag : 'Bindings'
}

{ #category : 'accessing - platform' }
SDL2Library >> macLibraryName [

	^ FFIMacLibraryFinder
		findAnyLibrary: #(
			'libSDL2.dylib'
			'libSDL2-2.0.0.dylib')
]

{ #category : 'accessing - platform' }
SDL2Library >> unix32LibraryName [

	^ FFIUnix32LibraryFinder findAnyLibrary: #(
		'libSDL2-2.0.so.0.2.1'
		'libSDL2-2.0.so.0')
]

{ #category : 'accessing - platform' }
SDL2Library >> unix64LibraryName [

	^ FFIUnix64LibraryFinder findAnyLibrary: #(
		'libSDL2-2.0.so.0.2.1'
		'libSDL2-2.0.so.0')
]

{ #category : 'accessing - platform' }
SDL2Library >> unixLibraryName [
	"Backward compatibility"
	^ self unix32LibraryName
]

{ #category : 'accessing - platform' }
SDL2Library >> win32LibraryName [

	^ FFIWindowsLibraryFinder findAnyLibrary: #('SDL2.dll' 'libSDL2.dll')
]
