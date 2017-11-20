"
A GLMBrickAbstractAdaptor is an interface for all adaptors to be used with brick lazy widgets
"
Class {
	#name : #GLMBrickAbstractModel,
	#superclass : #Object,
	#category : #Glamour-Morphic-Brick-Model
}

{ #category : #'adaptor api' }
GLMBrickAbstractModel >> count [

	self subclassResponsibility
]

{ #category : #'adaptor api' }
GLMBrickAbstractModel >> morphAt: anInteger convert: aMorph [

	self subclassResponsibility
]
