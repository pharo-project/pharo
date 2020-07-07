"
I'm an annotation to represent an environment (as in LaTeX).

Here is for example an environment called card. 
It is composed of a begin and end environment annotations.

```
${begin:card}$

!! I'm Cool

A paragraph about the card.
I will be interpreted as part of the card.

${end:card}$ 
```

See comment of PREnvironment. 




"
Class {
	#name : #PREnvironmentAnnotation,
	#superclass : #PRAbstractAnnotation,
	#category : #'Pillar-Core-Model'
}

{ #category : #'instance creation' }
PREnvironmentAnnotation class >> named: aString [ 
	
	^ self new name: aString; yourself
]

{ #category : #testing }
PREnvironmentAnnotation class >> possibleParameters [
	^ #(name)
]

{ #category : #testing }
PREnvironmentAnnotation class >> validateParameters: aPRParameters dictionary: parameters in: anInstance [

	"I do not do any validations.
	I allow arbitrary parameters that will be used by my transformers."
]

{ #category : #accessing }
PREnvironmentAnnotation >> name [

	^ self parameters at: 'name' ifAbsent: [ PREnvironmentMissedNamed signal ]
]

{ #category : #accessing }
PREnvironmentAnnotation >> name: aString [ 
	
	self parameters at: 'name' put: aString
]
