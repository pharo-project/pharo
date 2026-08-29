"
I provide the simplest baseline: the currently compiled source code of the method.
This shows changes since the method was last compiled/saved.

I serve as the default fallback when neither Git nor Epicea information
is available.
"
Class {
	#name : 'RubCompiledMethodBaselineProvider',
	#superclass : 'RubMethodBaselineProvider',
	#category : 'Rubric-Editing-Widgets',
	#package : 'Rubric',
	#tag : 'Editing-Widgets'
}

{ #category : 'accessing' }
RubCompiledMethodBaselineProvider >> baselineForMethod: aCompiledMethod [
	^ aCompiledMethod sourceCode
]
