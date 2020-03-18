Class {
	#name : #MorphicRenderLoop,
	#superclass : #Object,
	#category : #'Morphic-Core-Kernel'
}

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
