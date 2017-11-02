"
A GLMRenderer is the abstract class for the renderer hierarchy. This should be subclassed for each rendering platform (e.g. Morphic)
"
Class {
	#name : #GLMRenderer,
	#superclass : #Object,
	#category : 'Glamour-Core'
}

{ #category : #rendering }
GLMRenderer >> doOpen: aUI [ 
	 
	^self subclassResponsibility
]

{ #category : #rendering }
GLMRenderer >> open: aRenderable [ 
	 
	^self doOpen: (self render: aRenderable)
]

{ #category : #rendering }
GLMRenderer >> render: aRenderable [ 
	 
	^aRenderable renderGlamorouslyOn: self
]

{ #category : #rendering }
GLMRenderer >> renderPane: aPane [ 
	 
	^nil
]

{ #category : #rendering }
GLMRenderer >> renderPresentation: aPresentation [ 

	^ nil
]
