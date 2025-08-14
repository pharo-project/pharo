Class {
	#name : 'TFExternalFunctionFlags',
	#superclass : 'SharedPool',
	#classVars : [
		'PointersMightBeExternalAddresses',
		'PointersMightBeObjects',
		'UseOptimizedVersion'
	],
	#category : 'ThreadedFFI-Base',
	#package : 'ThreadedFFI',
	#tag : 'Base'
}

{ #category : 'class initialization' }
TFExternalFunctionFlags class >> initialize [

	UseOptimizedVersion					:= 1 << 0.
	PointersMightBeObjects				:= 1 << 1.
	PointersMightBeExternalAddresses	:= 1 << 2.

]

{ #category : 'class initialization' }
TFExternalFunctionFlags class >> pointersMightBeExternalAddresses [

	^ PointersMightBeExternalAddresses
]

{ #category : 'class initialization' }
TFExternalFunctionFlags class >> pointersMightBeObjects [

	^ PointersMightBeObjects
]

{ #category : 'class initialization' }
TFExternalFunctionFlags class >> useOptimizedVersion [

	^ UseOptimizedVersion
]
