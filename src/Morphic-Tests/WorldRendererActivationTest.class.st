"
I check that the activation of a world renderer tells it whether the image is starting.

## Design Rationale

- Only `UIManager class>>startUp:` knows that the image is starting. The flag goes down through `WorldState>>worldRenderer:whenImageStarting:` and `AbstractWorldRenderer>>activateWhenImageStarting:` to `doActivateWhenImageStarting:`, where a renderer can renew what did not survive the previous session. The former selectors, `worldRenderer:` and `activate`, tell that the image is not starting.
- I use a `WorldRendererMock`, which only remembers the flag: no window opens, so I run in a headless image too.
"
Class {
	#name : 'WorldRendererActivationTest',
	#superclass : 'TestCase',
	#instVars : [
		'renderer'
	],
	#category : 'Morphic-Tests-Kernel',
	#package : 'Morphic-Tests',
	#tag : 'Kernel'
}

{ #category : 'running' }
WorldRendererActivationTest >> setUp [

	super setUp.
	renderer := WorldRendererMock new
]

{ #category : 'tests' }
WorldRendererActivationTest >> testActivateTellsThatTheImageIsNotStarting [

	renderer activate.
	self deny: renderer imageStarting
]

{ #category : 'tests' }
WorldRendererActivationTest >> testActivateWhenImageStartingTellsTheRenderer [

	renderer activateWhenImageStarting: true.
	self assert: renderer imageStarting
]

{ #category : 'tests' }
WorldRendererActivationTest >> testWorldRendererTellsThatTheImageIsNotStarting [

	WorldState new worldRenderer: renderer.
	self deny: renderer imageStarting
]

{ #category : 'tests' }
WorldRendererActivationTest >> testWorldRendererWhenImageStartingTellsTheRenderer [

	WorldState new worldRenderer: renderer whenImageStarting: true.
	self assert: renderer imageStarting
]
