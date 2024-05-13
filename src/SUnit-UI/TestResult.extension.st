Extension { #name : 'TestResult' }

{ #category : '*SUnit-UI' }
TestResult class >> defaultColorBackGroundForErrorTest [

	^ Color red darker darker
]

{ #category : '*SUnit-UI' }
TestResult class >> defaultColorBackGroundForFailureTest [

	^ Color yellow darker darker darker
]

{ #category : '*SUnit-UI' }
TestResult class >> defaultColorBackGroundForPassingTest [

	^ Color green darker darker
]

{ #category : '*SUnit-UI' }
TestResult class >> defaultColorText [
	^ Color black
]

{ #category : '*SUnit-UI' }
TestResult class >> defaultColorTextForErrorTest [
	^ self defaultColorText
]

{ #category : '*SUnit-UI' }
TestResult class >> defaultColorTextForFailureTest [
	^ self defaultColorText
]

{ #category : '*SUnit-UI' }
TestResult class >> defaultColorTextForPassingTest [
	^ self defaultColorText
]
