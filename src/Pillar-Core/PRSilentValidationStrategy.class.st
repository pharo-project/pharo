Class {
	#name : #PRSilentValidationStrategy,
	#superclass : #PRValidationStrategy,
	#category : #'Pillar-Core-Validations'
}

{ #category : #'error handling' }
PRSilentValidationStrategy >> invalidParameterNames: aMissingKeys forAnnotation: anAnnotationClass withParameters: aPRParameters [
	aMissingKeys do: [ :eachKey | 
		aPRParameters children 
			detect: [ :eachParameter | eachParameter key = eachKey ]  
			ifFound: [ :aParameter | aParameter beInvalid ]
			ifNone: [ "ignore" ] ]
]

{ #category : #'error handling' }
PRSilentValidationStrategy >> missingAnnotationParametersIn: aPRAnnotation [ 
	aPRAnnotation beInvalid
]

{ #category : #'error handling' }
PRSilentValidationStrategy >> missingAnnotationSupport: aString [ 
	^ PRUndefinedAnnotation new tag: aString
]

{ #category : #'error handling' }
PRSilentValidationStrategy >> unexpectedParameterIn: anAnnotation [
	anAnnotation beInvalid
]
