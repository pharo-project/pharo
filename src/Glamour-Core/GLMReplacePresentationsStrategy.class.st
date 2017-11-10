"
This strategy replaces the presentations from the pane of the destination port.
"
Class {
	#name : #GLMReplacePresentationsStrategy,
	#superclass : #GLMPresentStrategy,
	#category : #Glamour-Core
}

{ #category : #transmitting }
GLMReplacePresentationsStrategy >> transmitIn: aContext [
	"it is important for the destination to be set after the presentation, 
	because when the presentation is a browser, we might forward the entity internally"
	self transmission destination hasPane ifTrue: [
		self transmission destination pane notingPresentationChangeDo: [
			self transmission destination pane clearIn: aContext.
			self transmission destination pane presentations: self presentations copy ]].

]
