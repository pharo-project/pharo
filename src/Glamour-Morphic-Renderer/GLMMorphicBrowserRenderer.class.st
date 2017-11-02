Class {
	#name : #GLMMorphicBrowserRenderer,
	#superclass : #GLMMorphicWidgetRenderer,
	#category : 'Glamour-Morphic-Renderer'
}

{ #category : #callbacks }
GLMMorphicBrowserRenderer >> actOnBrowserClosed: ann [
	self renderer window delete
]
