"
Mockup of the annotation for math in text. 
We do not have a syntax from the pillar side  but this is for Microdown where I correspond to `$ math here $`.
May be this is better to have a subclass of PRFormat.
We will have to revisit this.
"
Class {
	#name : #PRMathInTextAnnotation,
	#superclass : #PRAbstractAnnotation,
	#category : #'Pillar-Core-Model'
}

{ #category : #protected }
PRMathInTextAnnotation class >> tag [
	"For now this is unclear how we will manage it. This is just to make sure that microdown mathInText is converted"
	^ #math
]

{ #category : #visiting }
PRMathInTextAnnotation >> accept: aVisitor [
	^ aVisitor visitMathInTextAnnotation: self
]
