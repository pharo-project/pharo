"
An extention of the RBTransformationRule to store Custom Rewrite Rules.
"
Class {
	#name : #RBCustomTransformationRule,
	#superclass : #RBTransformationRule,
	#instVars : [
		'input',
		'output',
		'searchFor',
		'replaceWith'
	],
	#category : #'RewriteRulesCreator-Core'
}

{ #category : #accessing }
RBCustomTransformationRule >> input [
	^ input
]

{ #category : #accessing }
RBCustomTransformationRule >> input: anObject [
	input := anObject
]

{ #category : #accessing }
RBCustomTransformationRule >> output [
	^ output
]

{ #category : #accessing }
RBCustomTransformationRule >> output: anObject [
	output := anObject
]

{ #category : #accessing }
RBCustomTransformationRule >> replaceWith [
	^ replaceWith
]

{ #category : #accessing }
RBCustomTransformationRule >> replaceWith: anObject [
	replaceWith := anObject
]

{ #category : #accessing }
RBCustomTransformationRule >> searchFor [
	^ searchFor
]

{ #category : #accessing }
RBCustomTransformationRule >> searchFor: anObject [
	searchFor := anObject
]
