Class {
	#name : #PRValidationStrategy,
	#superclass : #Object,
	#classInstVars : [
		'uniqueInstance'
	],
	#category : #'Pillar-Core-Validations'
}

{ #category : #cleanup }
PRValidationStrategy class >> cleanUp [
	self reset
]

{ #category : #accessing }
PRValidationStrategy class >> reset [
	uniqueInstance := nil
]

{ #category : #accessing }
PRValidationStrategy class >> shouting [
	^ PRShoutingValidationStrategy uniqueInstance
]

{ #category : #accessing }
PRValidationStrategy class >> silent [
	^ PRSilentValidationStrategy uniqueInstance
]

{ #category : #accessing }
PRValidationStrategy class >> uniqueInstance [
	^ uniqueInstance ifNil: [ uniqueInstance := self new ]
]

{ #category : #'error handling' }
PRValidationStrategy >> invalidParameterNames: aMissingKeys forAnnotation: anAnnotation withParameters: aPRParameters [
	^ self subclassResponsibility
]

{ #category : #'error handling' }
PRValidationStrategy >> missingAnnotationParametersIn: anAnnotation [
	^ self subclassResponsibility
]

{ #category : #'error handling' }
PRValidationStrategy >> missingAnnotationSupport: aString [ 
	^ self subclassResponsibility
]

{ #category : #'error handling' }
PRValidationStrategy >> unexpectedParameterIn: anAnnotation [
	self subclassResponsibility
]
