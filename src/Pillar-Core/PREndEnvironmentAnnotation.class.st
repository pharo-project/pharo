"
I'm the beginning of annotation to represent an environment (as in LaTeX): typically ${end:card}$ 
in the following example.

Here is for example an environment called card. 
It is composed of a begin and end environment annotations.

${begin:card}$

!! I'm Cool

A paragraph about the card.
I will be interpreted as part of the card.

${end:card}$ 

See comment of PREnvironment. 
"
Class {
	#name : #PREndEnvironmentAnnotation,
	#superclass : #PREnvironmentAnnotation,
	#category : #'Pillar-Core-Model'
}

{ #category : #protected }
PREndEnvironmentAnnotation class >> tag [ 
	^ #end
]

{ #category : #visiting }
PREndEnvironmentAnnotation >> accept: aVisitor [
	^ aVisitor visitEndEnvironmentAnnotation: self
]
