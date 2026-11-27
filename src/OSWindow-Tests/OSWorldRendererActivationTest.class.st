"
I check that `OSWorldRenderer>>doActivate`, sent when a native window opens, tells the renderer that the image is not starting, so that its driver keeps the running event loop.

## Design Rationale

- I use an `OSWorldRendererMock`, which only remembers the flag: no OS window opens, so I run in a headless image too.
"
Class {
	#name : 'OSWorldRendererActivationTest',
	#superclass : 'TestCase',
	#category : 'OSWindow-Tests-Tests',
	#package : 'OSWindow-Tests',
	#tag : 'Tests'
}

{ #category : 'tests' }
OSWorldRendererActivationTest >> testDoActivateTellsThatTheImageIsNotStarting [

	| renderer |
	renderer := OSWorldRendererMock new.
	renderer doActivate.
	self deny: renderer imageStarting
]
