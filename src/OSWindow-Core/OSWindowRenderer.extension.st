Extension { #name : #OSWindowRenderer }

{ #category : #'*OSWindow-Core-Extras' }
OSWindowRenderer >> onRenderThread: aBlock [
	OSWindowRenderThread enqueueOperation: aBlock
]

{ #category : #'*OSWindow-Core-Extras' }
OSWindowRenderer >> onRenderThreadBlocking: aBlock [
	OSWindowRenderThread enqueueBlockingOperation: aBlock
]
