Extension { #name : #OSSDL2BackendWindow }

{ #category : #'*Deprecated90' }
OSSDL2BackendWindow >> convertCursor: cursor withMask: mask [

	self deprecated: 'Use #convertCursor:withMask:andScale:'.

	^ self convertCursor: cursor withMask: mask andScale: 1
]
