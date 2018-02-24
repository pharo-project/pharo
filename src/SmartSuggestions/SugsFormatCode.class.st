"
I know how to format the code
"
Class {
	#name : #SugsFormatCode,
	#superclass : #SugsSuggestion,
	#category : #'SmartSuggestions-Suggestion'
}

{ #category : #execution }
SugsFormatCode >> execute [
	context formatSourceCode
]

{ #category : #display }
SugsFormatCode >> icon [
	^self iconNamed: #smallJustifiedIcon
]

{ #category : #display }
SugsFormatCode >> keyText [
	^'u'
]

{ #category : #display }
SugsFormatCode >> label [
	^'Format method' translated
]

{ #category : #display }
SugsFormatCode >> position [
	^0
]
