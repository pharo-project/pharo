"
LibTTY’s #uniqueInstance provides access to ‘libtty’, a library included with the Pharo VM that offers a function to spawn a process connected to a pseudo-terminal.
"
Class {
	#name : 'LibTTY',
	#superclass : 'FFILibrary',
	#pools : [
		'LibCTypes'
	],
	#category : 'UnifiedFFI-Libraries',
	#package : 'UnifiedFFI',
	#tag : 'Libraries'
}

{ #category : 'accessing - platform' }
LibTTY >> macLibraryName [

	^ 'libtty.dylib'
]

{ #category : 'convenience' }
LibTTY >> ttySpawn: fd path: path arguments: argumentsArray environment: environmentDictionary [

	| environmentArray |

	environmentArray := Array streamContents: [ :stream |
		environmentDictionary keysAndValuesDo: [ :key :value |
			stream nextPut: ('=' join: { key. value }) ] ].
	^ self valueWithNullTerminatedExternalArrayFromStrings: argumentsArray of: [ :argv |
		self valueWithNullTerminatedExternalArrayFromStrings: environmentArray of: [ :envp |
			self ttySpawn: fd path: path argv: argv envs: envp ] ]
]

{ #category : 'api' }
LibTTY >> ttySpawn: fdm path: path argv: argv envs: envp [

	^ self ffiCall: #(pid_t tty_spawn(int fdm, const char* path, void* argv, void* envp))
]

{ #category : 'accessing - platform' }
LibTTY >> unixLibraryName [

	^ 'libtty.so'
]

{ #category : 'private' }
LibTTY >> valueWithNullTerminatedExternalArrayFromStrings: strings of: block [

	| externalArray |

	externalArray := FFIExternalArray externalNewType: 'char*' size: strings size + 1.
	strings withIndexDo: [ :string :stringIndex |
		| byteArray externalAddress |
		byteArray := string utf8Encoded.
		externalAddress := ExternalAddress allocate: byteArray size + 1.
		byteArray withIndexDo: [ :byte :byteIndex |
			externalAddress byteAt: byteIndex put: byte ].
		externalAddress byteAt: byteArray size + 1 put: 0.
		externalArray at: stringIndex put: externalAddress ].
	externalArray at: strings size + 1 put: ExternalAddress null.
	^ [ block value: externalArray ] ensure: [
		1 to: strings size do: [ :index |
			(externalArray at: index) free ].
		externalArray free ]
]

{ #category : 'accessing - platform' }
LibTTY >> win32LibraryName [

	self shouldNotImplement
]
