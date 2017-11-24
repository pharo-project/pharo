"
I'm in charge to style an ast when there are semantic warnings
"
Class {
	#name : #SemanticWarningIconStyler,
	#superclass : #IconStyler,
	#category : #'Reflectivity-Tools-Breakpoints'
}

{ #category : #defaults }
SemanticWarningIconStyler >> highlightColor [
	^(Color yellow alpha: 0.3)
]

{ #category : #defaults }
SemanticWarningIconStyler >> iconFor: aNode [
	^self iconNamed: #flag.
]

{ #category : #defaults }
SemanticWarningIconStyler >> iconLabel: aNode [
	^'Semantic warning: ', (aNode propertyAt: #semanticWarning) asString
]

{ #category : #testing }
SemanticWarningIconStyler >> shouldStyleNode: aNode [
	^aNode hasProperty: #semanticWarning
]
