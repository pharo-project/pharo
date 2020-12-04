Class {
	#name : #MorphicRenderLoop,
	#superclass : #Object,
	#category : #'Morphic-Core-Kernel'
}

{ #category : #'deferred message' }
MorphicRenderLoop >> defer: aValuable [

	WorldState addDeferredUIMessage: aValuable
]

{ #category : #'initialize-release' }
MorphicRenderLoop >> doOneCycle [

	WorldMorph doOneCycle
]

{ #category : #'initialize-release' }
MorphicRenderLoop >> doOneCycleWhile: aBlock [

	[ aBlock value ] whileTrue: [ 
		self doOneCycle.
		"Collaborate with other processes in the same priority"
		Processor yield. ]
]
