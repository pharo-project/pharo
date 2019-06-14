"
A filter for selectors
"
Class {
	#name : #SelectorFilter,
	#superclass : #StackFilter,
	#instVars : [
		'selectors'
	],
	#category : #'Debugger-Filters'
}

{ #category : #'instance creation' }
SelectorFilter class >> forSelector: aSelector [
	|filter|
	
	filter:= self new.
	filter selectors add: aSelector.
	^ filter
]

{ #category : #'instance creation' }
SelectorFilter class >> forSelectors: someSelectors [
	 |filter|
	
	 filter:= self new.
	 filter selectors addAll: someSelectors.
	^ filter
]

{ #category : #initialization }
SelectorFilter >> initialize [
	super initialize.
	self selectors: OrderedCollection new.
]

{ #category : #accessing }
SelectorFilter >> selectors [
 	^selectors
]

{ #category : #accessing }
SelectorFilter >> selectors: someSelectors [
	selectors := someSelectors
]

{ #category : #testing }
SelectorFilter >> shouldDisplay: aContext [
	 ^ (self selectors  
		includes: aContext method selector) not.
]
