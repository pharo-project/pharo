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

{ #category : 'api' }
LibTTY >> ttySpawn: fdm path: path argv: argv envs: envp [

	^ self ffiCall: #(pid_t tty_spawn(int fdm, const char* path, void* argv, void* envp))
]

{ #category : 'accessing - platform' }
LibTTY >> unixLibraryName [

	^ 'libtty.so'
]

{ #category : 'accessing - platform' }
LibTTY >> win32LibraryName [

	self shouldNotImplement
]
