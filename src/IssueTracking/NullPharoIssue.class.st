"
I am an instance of the Null Object Pattern [1] so that users of PharoIssue do not have to put nil checks everywhere.

1. http://en.wikipedia.org/wiki/Null_Object_pattern
"
Class {
	#name : #NullPharoIssue,
	#superclass : #Object,
	#category : #'IssueTracking-Base'
}

{ #category : #accessing }
NullPharoIssue >> number [
	^ nil
]

{ #category : #accessing }
NullPharoIssue >> title [
	^ nil
]
