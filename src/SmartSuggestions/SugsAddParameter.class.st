"
I know how to add a parameter to a method
"
Class {
	#name : #SugsAddParameter,
	#superclass : #SugsSuggestion,
	#category : #'SmartSuggestions-Suggestion'
}

{ #category : #accessing }
SugsAddParameter class >> nodes [
	^{RBMethodNode}
]

{ #category : #execution }
SugsAddParameter >> execute [
	(NautilusRefactoring model: context) addAParameterFor: context selectedMethod
]

{ #category : #display }
SugsAddParameter >> icon [
	^self iconNamed: #smallPushpinIcon
]

{ #category : #display }
SugsAddParameter >> label [
	^'Add a parameter' translated
]

{ #category : #display }
SugsAddParameter >> position [
	^200
]
