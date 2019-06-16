"
A filter for booleans
"
Class {
	#name : #BooleanFilter,
	#superclass : #StackFilter,
	#instVars : [
		'filters',
		'booleanOperator'
	],
	#category : #'Debugger-Filters'
}

{ #category : #'instance creation' }
BooleanFilter class >> withFilters: someFilters operator: booleanOperator [
	^self new 
		filters: someFilters; 
		booleanOperator: booleanOperator
]

{ #category : #accessing }
BooleanFilter >> booleanOperator [
	^booleanOperator
]

{ #category : #accessing }
BooleanFilter >> booleanOperator: aSymbol [
	booleanOperator:= aSymbol
]

{ #category : #accessing }
BooleanFilter >> filters [
	^filters
]

{ #category : #accessing }
BooleanFilter >> filters: theFilters [
	filters:= theFilters
]

{ #category : #testing }
BooleanFilter >> shouldDisplay: aContext [
	^ (self filters first shouldDisplay: aContext) perform: booleanOperator with: [self filters last shouldDisplay: aContext]  
]
