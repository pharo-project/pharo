"
This strategy sets presentations only if none exits in the pane of the destination port.
"
Class {
	#name : #GLMPresentIfNoneStrategy,
	#superclass : #GLMPresentStrategy,
	#category : #Glamour-Core
}

{ #category : #transmitting }
GLMPresentIfNoneStrategy >> transmitIn: aContext [
	(self transmission destination hasPane and: [
		self transmission destination pane presentations isEmpty]) ifTrue: [	
			self transmission destination pane
				notingPresentationChangeDo: [
					self transmission destination pane clearIn: aContext.
					self transmission destination pane presentations: self presentations copy ] ]
]
