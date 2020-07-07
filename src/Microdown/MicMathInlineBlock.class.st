"
Microdown supports in text math expression as in LaTeX using `$ 1 + 2 $`. Each exporter is then responsible to map the contents to the correct output. For PDF based on LaTeX mathInlineBlock can contain LaTeX, for HTML we expect the same using corresponding math library in JS.

Here is an example $V_i = C_0 - C_3$

"
Class {
	#name : #MicMathInlineBlock,
	#superclass : #MicAbstractInlineBlock,
	#category : #'Microdown-ModelInline'
}

{ #category : #testing }
MicMathInlineBlock class >> isEvaluated [
	^ false
]
