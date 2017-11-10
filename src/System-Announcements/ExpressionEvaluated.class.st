"
This announcement correspond to code evaluation.  For example, a DoIt or PrintIt evaluated in a workspace raises one of this announcements.
"
Class {
	#name : #ExpressionEvaluated,
	#superclass : #SystemAnnouncement,
	#instVars : [
		'context',
		'expressionEvaluated'
	],
	#category : #System-Announcements-System-Base
}

{ #category : #'instance creation' }
ExpressionEvaluated class >> expression: theEvaluatedExpression [
	^self expression: theEvaluatedExpression context: nil
]

{ #category : #'instance creation' }
ExpressionEvaluated class >> expression: theEvaluatedExpression context: anExecutionContext [
	^self new
			expressionEvaluated: theEvaluatedExpression;
			context: anExecutionContext;
			yourself
]

{ #category : #accessing }
ExpressionEvaluated >> context [
	
	^ context
]

{ #category : #accessing }
ExpressionEvaluated >> context: anObject [
	
	context := anObject
]

{ #category : #'temporal for remove' }
ExpressionEvaluated >> expression [
	^self expressionEvaluated
]

{ #category : #accessing }
ExpressionEvaluated >> expressionEvaluated [
	
	^ expressionEvaluated
]

{ #category : #accessing }
ExpressionEvaluated >> expressionEvaluated: anObject [
	
	expressionEvaluated := anObject
]

{ #category : #'temporal for remove' }
ExpressionEvaluated >> item [
	^expressionEvaluated
]
