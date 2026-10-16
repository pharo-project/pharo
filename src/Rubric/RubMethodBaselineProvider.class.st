"
I am an abstract baseline provider for RubGutterDiffDisplayer.
Subclasses implement #baselineForMethod: to return the original source code
for a compiled method, against which the current editor text is compared.

I support a pluggable fallback chain via #fallbackProvider:.
"
Class {
	#name : 'RubMethodBaselineProvider',
	#superclass : 'Object',
	#instVars : [
		'fallbackProvider'
	],
	#category : 'Rubric-Editing-Widgets',
	#package : 'Rubric',
	#tag : 'Editing-Widgets'
}

{ #category : 'accessing' }
RubMethodBaselineProvider >> baselineForMethod: aCompiledMethod [

	self subclassResponsibility
]

{ #category : 'accessing' }
RubMethodBaselineProvider >> baselineForMethod: aCompiledMethod ifNil: aBlock [

	| result |

	result := self baselineForMethod: aCompiledMethod.

	^ result ifNil: aBlock
]

{ #category : 'accessing' }
RubMethodBaselineProvider >> baselineForMethod: aCompiledMethod ifNone: aBlock [

	^ self
		  baselineForMethod: aCompiledMethod
		  ifNil: [ fallbackProvider ifNotNil: [
				fallbackProvider baselineForMethod: aCompiledMethod ifNone: aBlock
	] ifNil: aBlock ]
]

{ #category : 'accessing' }
RubMethodBaselineProvider >> fallbackProvider [

	^ fallbackProvider
]

{ #category : 'accessing' }
RubMethodBaselineProvider >> fallbackProvider: aProvider [

	fallbackProvider := aProvider
]
