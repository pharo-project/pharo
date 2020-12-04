"
OSWindowHandle was deprecated in Pharo 9 in favor of OSBackendWindow.
Replace your usages.

For more information, please check the comment in OSBackendWindow.
"
Class {
	#name : #OSWindowHandle,
	#superclass : #OSBackendWindow,
	#category : #'OSWindow-Core-Utilities'
}

{ #category : #testing }
OSWindowHandle class >> isDeprecated [

	^ true
]
