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
	#category : #'Microdown-Model'
}

{ #category : #visiting }
MicMathBlock >> accept: aVisitor [ 
 	^ aVisitor visitMath: self
]

{ #category : #accessing }
MicMathBlock >> arguments [
	self flag: #strange.
	"why this is not called directly during the closeMe phase."
	^ arguments ifNil: [ arguments := ZnResourceMetaUtils parseQueryFrom: firstLine readStream ]
]

{ #category : #markups }
MicMathBlock >> lineStartMarkup [
	
	^ parser mathOpeningBlockMarkup
]
