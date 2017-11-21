"
I know how to render a Hiedra ruler element in an Athens canvas.

I have an Athens canvas and my concrete subclasses know how to #render.
"
Class {
	#name : #HiRenderer,
	#superclass : #Object,
	#instVars : [
		'athensCanvas'
	],
	#category : #Hiedra-Rendering
}

{ #category : #accessing }
HiRenderer >> athensCanvas: anAthensCanvas [ 
	athensCanvas := anAthensCanvas 
]

{ #category : #rendering }
HiRenderer >> render [
	"Render a Hiedra ruler element in the athensCanvas"
	
	self subclassResponsibility
]
