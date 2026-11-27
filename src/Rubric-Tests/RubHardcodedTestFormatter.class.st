"
I am a formatter for Rubric's tests. I answer the same formatted code whatever I am given, so a test knows what formatting produces.
"
Class {
	#name : 'RubHardcodedTestFormatter',
	#superclass : 'Object',
	#category : 'Rubric-Tests',
	#package : 'Rubric-Tests'
}

{ #category : 'formatting' }
RubHardcodedTestFormatter >> format: aNode [

	^ 'method

	(to
		 format;
		 but) maybe not nice'
]
