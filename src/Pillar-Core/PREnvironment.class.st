"
I represent an environment (as in LaTeX).
I'm a `PRDocumentGroup` with a name. 
Syntactically I'm delimited by a start and an end annotation. 
I can be nested inside other environment. 

```
${begin:card}$

!! I'm Cool

A paragraph about the card.
I will be interpreted as part of the card.

${end:card}$ 
```

### Known Limits
For now the parameters should be tested and available from the syntax and this is probably not the case. 

"
Class {
	#name : #PREnvironment,
	#superclass : #PRDocumentGroup,
	#traits : 'PRTParametrizable',
	#classTraits : 'PRTParametrizable classTrait',
	#instVars : [
		'name',
		'parameters'
	],
	#category : #'Pillar-Core-Model'
}

{ #category : #'instance creation' }
PREnvironment class >> named: aString [ 
	
	^ self new name: aString; yourself
]

{ #category : #visiting }
PREnvironment >> accept: aVisitor [
	^ aVisitor visitEnvironment: self
]

{ #category : #initialization }
PREnvironment >> initialize [
	super initialize.
	parameters := OrderedDictionary new
]

{ #category : #accessing }
PREnvironment >> name [
	^ name
]

{ #category : #accessing }
PREnvironment >> name: aString [ 
	
	name := aString
]

{ #category : #'accessing - parameters' }
PREnvironment >> parameters [
	^ parameters
]

{ #category : #visiting }
PREnvironment >> printOn: aStream [
	
	super printOn: aStream.
	aStream nextPut: $(; print: self name; nextPut: $)
]
