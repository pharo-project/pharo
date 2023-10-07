"
I'm in charge to style an ast when there are semantic warnings
"
Class {
	#name : 'SemanticWarningIconStyler',
	#superclass : 'IconStyler',
	#category : 'Reflectivity-Tools-Breakpoints',
	#package : 'Reflectivity-Tools',
	#tag : 'Breakpoints'
}

{ #category : 'testing' }
SemanticWarningIconStyler class >> isStaticStyler [

	^ true
]

{ #category : 'defaults' }
SemanticWarningIconStyler >> highlightColor [
	^(Color yellow alpha: 0.3)
]

{ #category : 'defaults' }
SemanticWarningIconStyler >> iconFor: aNode [
	^self iconNamed: #warning
]

{ #category : 'defaults' }
SemanticWarningIconStyler >> iconLabel: aNode [

	self flag: 'What if more than one warning?'.
	^ aNode warningNotices first messageText
]

{ #category : 'testing' }
SemanticWarningIconStyler >> shouldStyleNode: aNode [
	^aNode warningNotices isNotEmpty
]
