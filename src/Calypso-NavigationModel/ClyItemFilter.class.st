"
I am a root of hierarchy of filters.

My subclasses must implement method #matches: to check if given object satisfies specific condition.

My instances are used by various types of queries. For example ClyFilterQuery select subset of actual query result using some of my subclasses.

So as part of query I am participate in the environment query cache. And therefore my subclasses should correctly implement comparison methods (equality and hash) when they are pluggable by any parameter
"
Class {
	#name : #ClyItemFilter,
	#superclass : #Object,
	#category : #'Calypso-NavigationModel-Model'
}

{ #category : #testing }
ClyItemFilter >> matches: anEnvironmentItem [
	self subclassResponsibility 
]

{ #category : #printing }
ClyItemFilter >> printCriteria [

	^self printString 
]
