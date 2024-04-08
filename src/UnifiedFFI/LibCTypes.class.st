"
LibCTypes defines types used in `LibC`.
"
Class {
	#name : 'LibCTypes',
	#superclass : 'SharedPool',
	#classVars : [
		'pid_t'
	],
	#category : 'UnifiedFFI-Libraries',
	#package : 'UnifiedFFI',
	#tag : 'Libraries'
}

{ #category : 'class initialization' }
LibCTypes class >> initialize [

	pid_t := #int.
]
