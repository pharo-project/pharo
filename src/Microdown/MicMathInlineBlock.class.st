"
Microdown supports in text math expression as in LaTeX using `$ 1 + 2 $`. Each exporter is then responsible to map the contents to the correct output. For PDF based on LaTeX mathInlineBlock can contain LaTeX, for HTML we expect the same using corresponding math library in JS.

Here are several examples:

#### Example 1
Code: `$V_i = C_0 - C_3$`

$V_i = C_0 - C_3$

#### Example 2
Code: `$a^2 + b^2 = c^2$`

$a^2 + b^2 = c^2$

#### Example 3
Code: `$\\sin(x) = \\sum_{n=0}^{\\infty} \\frac{(-1)^{n}x^{2n+1}}{(2n+1)!}$`

$\\sin(x) = \\sum_{n=0}^{\\infty} \\frac{(-1)^{n}x^{2n+1}}{(2n+1)!}$

#### Example 4
Code: `$f(a) = \\frac{1}{2\\pi i} \\oint_{\\gamma} \\frac{f(z)}{z - a} dz$`

$f(a) = \\frac{1}{2\\pi i} \\oint_{\\gamma} \\frac{f(z)}{z - a} dz$

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
