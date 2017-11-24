Class {
	#name : #MorphicLabelAdapterTest,
	#superclass : #TestCase,
	#instVars : [
		'label'
	],
	#category : #'Spec-Tests'
}

{ #category : #accessing }
MorphicLabelAdapterTest >> modelClass [
	^ LabelPresenter
]

{ #category : #running }
MorphicLabelAdapterTest >> setUp [
	label := self modelClass new
]

{ #category : #tests }
MorphicLabelAdapterTest >> testEmphasisBold [
	label emphasis: #bold.
	label buildWithSpec.
	self assert: label widget widget font emphasis equals: 1
]

{ #category : #tests }
MorphicLabelAdapterTest >> testEmphasisMultiple [
	label emphasis: #(bold italic).
	label buildWithSpec.
	self assert: label widget widget font emphasis equals: 3
]

{ #category : #tests }
MorphicLabelAdapterTest >> testEmphasisNone [
	label buildWithSpec.
	self assert: label widget widget font emphasis equals: 0
]
