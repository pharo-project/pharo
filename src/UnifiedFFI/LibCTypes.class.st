"
LibCTypes defines types used in `LibC`.
"
Class {
	#name : 'LibCTypes',
	#superclass : 'SharedPool',
	#classVars : [
		'pid_t',
		'ssize_t'
	],
	#category : 'UnifiedFFI-Libraries',
	#package : 'UnifiedFFI',
	#tag : 'Libraries'
}

{ #category : 'class initialization' }
LibCTypes class >> initialize [

	ssize_t := #long.
	pid_t := #int.
]
