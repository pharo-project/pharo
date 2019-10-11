Class {
	#name : #SDL2Library,
	#superclass : #FFILibrary,
	#category : #'OSWindow-SDL2-Bindings'
}

{ #category : #'nb-library' }
SDL2Library >> calloutAPIClass [ 

	^ TFCalloutAPI
]

{ #category : #'accessing platform' }
SDL2Library >> checkLibraryName: aName [

	^ (ExternalAddress loadSymbol: 'SDL_Init' from: aName) notNil
]

{ #category : #'accessing platform' }
SDL2Library >> findSDL2 [

	"Look for SDL2 using different names."
	#(0							"Static"
	SDL2
	'libSDL2-2.0.0.dylib'		"Mac"
	'libSDL2-2.0.so.0'			"Linux 1"
	'libSDL2-2.0.so.0.2.1'	"Linux 2"
	'SDL2.dll'					"Windows"
	) do: [ :eachName | 
		[ (self checkLibraryName: eachName) ifTrue: [ ^ eachName ] ] 
		ifError: [nil] ].
	self error: 'Failed to find SDL2 library.'
]

{ #category : #'accessing platform' }
SDL2Library >> macModuleName [ 
	^ self findSDL2 
]

{ #category : #'nb-library' }
SDL2Library >> runner [

	^ TFMainThreadRunner uniqueInstance
]
