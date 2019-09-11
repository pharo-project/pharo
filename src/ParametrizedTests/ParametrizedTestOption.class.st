"
An option reifies the relationship in between a selector / property with it potential values. 
"
Class {
	#name : #ParametrizedTestOption,
	#superclass : #Object,
	#instVars : [
		'selector',
		'values'
	],
	#category : #'ParametrizedTests-Core'
}

{ #category : #accessing }
ParametrizedTestOption >> expand [
	^ values
		collect: [ :each | 
			ParametrizedTestExpandedParameter new
				selector: selector;
				valuable: each ]
]

{ #category : #accessing }
ParametrizedTestOption >> selector [
	^ selector
]

{ #category : #accessing }
ParametrizedTestOption >> selector: anObject [
	selector := anObject
]

{ #category : #accessing }
ParametrizedTestOption >> values [
	^ values
]

{ #category : #accessing }
ParametrizedTestOption >> values: anObject [
	values := anObject
]
