"
I'm a math environment. I'm a block it means that I start by a newline with character `&` and optional parameters, and I finish by a line with the same character. 
My body can be on multiple line and may contain LaTeX expressions.

Here is an example 

```
&
V_i = C_0 - C_3
&
```

produces 

&
V_i = C_0 - C_3
&


"
Class {
	#name : #MicMathBlock,
	#superclass : #MicSameStartStopMarkupBlock,
	#instVars : [
		'arguments'
	],
	#category : #'Microdown-Model'
}

{ #category : #accessing }
MicMathBlock >> arguments [
	
	^ arguments ifNil: [ arguments := ZnResourceMetaUtils parseQueryFrom: firstLine readStream ]
]

{ #category : #markups }
MicMathBlock >> lineStartMarkup [
	
	^ parser mathOpeningBlockMarkup 
]
