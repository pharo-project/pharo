Class {
	#name : #MorphicTreeAdapterTest,
	#superclass : #TestCase,
	#instVars : [
		'treeWithItems'
	],
	#category : #'Spec-Tests'
}

{ #category : #accessing }
MorphicTreeAdapterTest >> modelClass [
	^ TreePresenter
]

{ #category : #running }
MorphicTreeAdapterTest >> setUp [
	treeWithItems := self modelClass new.
	treeWithItems roots: #(#first #second #third #fourth #fifth)
]

{ #category : #running }
MorphicTreeAdapterTest >> testDefaultRowColors [
	| defaultColor |
	defaultColor := Smalltalk ui theme backgroundColor.
	treeWithItems buildWithSpec.
	self
		assert: treeWithItems widget widget rowColors
		equals: (Array with: defaultColor with: defaultColor)
]

{ #category : #running }
MorphicTreeAdapterTest >> testRowColorsAfterBuild [
	treeWithItems buildWithSpec.
	treeWithItems oddRowColor: Color red.
	treeWithItems evenRowColor: Color green.
	self
		assert: treeWithItems widget widget rowColors
		equals: (Array with: Color green with: Color red)
]

{ #category : #running }
MorphicTreeAdapterTest >> testRowColorsBeforeBuild [
	treeWithItems oddRowColor: Color red.
	treeWithItems evenRowColor: Color green.
	treeWithItems buildWithSpec.
	self
		assert: treeWithItems widget widget rowColors
		equals: (Array with: Color green with: Color red)
]
