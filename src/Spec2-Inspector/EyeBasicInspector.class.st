"
I am a special inspector that only lists the real fields of an object. I have exactly the same behavior as my superclass but I am used as a placeholder for the the different inspector views in the InspectorNavigator.
"
Class {
	#name : #EyeBasicInspector,
	#superclass : #EyeInspector,
	#category : #'Spec2-Inspector-Base'
}

{ #category : #accessing }
EyeBasicInspector class >> label [
	^ 'Basic Inspector'
]
