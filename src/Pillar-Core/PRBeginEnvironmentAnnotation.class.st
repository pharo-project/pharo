"
I'm the beginning of annotation to represent an environment (as in LaTeX): typically ${begin:card}$ 
in the following example.

Here is for example an environment called card. 
It is composed of a begin and end environment annotations.

```language=Smalltalk
${begin:card}$

!! I'm Cool

A paragraph about the card.
I will be interpreted as part of the card.

${end:card}$ 
```

See comment of PREnvironment. 
"
Class {
	#name : #PRBeginEnvironmentAnnotation,
	#superclass : #PREnvironmentAnnotation,
	#category : #'Pillar-Core-Model'
}

{ #category : #protected }
PRBeginEnvironmentAnnotation class >> tag [
	^ #begin
]

{ #category : #visiting }
PRBeginEnvironmentAnnotation >> accept: aVisitor [
	^ aVisitor visitBeginEnvironmentAnnotation: self
]
