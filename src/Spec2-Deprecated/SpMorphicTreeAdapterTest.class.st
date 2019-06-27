"
SUnit tests for MorphicTreeAdapter
"
Class {
	#name : #SpMorphicTreeAdapterTest,
	#superclass : #TestCase,
	#instVars : [
		'treeWithItems'
	],
	#category : #'Spec2-Deprecated-Tests'
}

{ #category : #deprecation }
SpMorphicTreeAdapterTest class >> isDeprecated [
	^ true
]

{ #category : #accessing }
SpMorphicTreeAdapterTest >> modelClass [
	^ SpTreePresenter
]

{ #category : #running }
SpMorphicTreeAdapterTest >> setUp [
	super setUp.
	treeWithItems := self modelClass new.
	treeWithItems roots: #(#first #second #third #fourth #fifth)
]

{ #category : #running }
SpMorphicTreeAdapterTest >> testDefaultRowColors [
	| defaultColor |
	defaultColor := Smalltalk ui theme backgroundColor.
	treeWithItems buildWithSpec.
	self
		assert: treeWithItems adapter widget rowColors
		equals: (Array with: defaultColor with: defaultColor)
]

{ #category : #running }
SpMorphicTreeAdapterTest >> testRowColorsAfterBuild [
	treeWithItems buildWithSpec.
	treeWithItems oddRowColor: Color red.
	treeWithItems evenRowColor: Color green.
	self
		assert: treeWithItems adapter widget rowColors
		equals: (Array with: Color green with: Color red)
]

{ #category : #running }
SpMorphicTreeAdapterTest >> testRowColorsBeforeBuild [
	treeWithItems oddRowColor: Color red.
	treeWithItems evenRowColor: Color green.
	treeWithItems buildWithSpec.
	self
		assert: treeWithItems adapter widget rowColors
		equals: (Array with: Color green with: Color red)
]
