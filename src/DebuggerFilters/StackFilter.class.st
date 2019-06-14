"
A filter on the current stack for debugging
"
Class {
	#name : #StackFilter,
	#superclass : #Object,
	#category : #'Debugger-Filters'
}

{ #category : #operators }
StackFilter >> and: aFilter [
	  ^ BooleanFilter withFilters: (OrderedCollection with: self with: aFilter) operator: #and:
]

{ #category : #operators }
StackFilter >> or: aFilter [
	  ^ BooleanFilter withFilters: (OrderedCollection with: self with: aFilter) operator: #or:
]

{ #category : #testing }
StackFilter >> shouldDisplay: aContext [
	^self subclassResponsibility 
]
