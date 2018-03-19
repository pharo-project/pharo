"
I know how to deprecate method.

"
Class {
	#name : #SugsDeprecateMethod,
	#superclass : #SugsSuggestion,
	#category : #'SmartSuggestions-Suggestion'
}

{ #category : #accessing }
SugsDeprecateMethod class >> nodes [
	^{RBMethodNode}
]

{ #category : #execution }
SugsDeprecateMethod >> execute [
	(NautilusRefactoring model: context) deprecateMethodFor: context selectedMethod
]

{ #category : #display }
SugsDeprecateMethod >> icon [
	^self iconNamed: #smallLanguageIcon
]

{ #category : #display }
SugsDeprecateMethod >> label [
	^ 'Deprecate' translated
]

{ #category : #display }
SugsDeprecateMethod >> position [
	^125
]
