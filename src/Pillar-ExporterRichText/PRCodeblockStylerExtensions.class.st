"
!!! Purpose
I am a class that provide Pillar codebloc extensions for Latex (and others in the future).

That mean you can write latex formulas using a codeblock ==language=latexMath==:


[[[language=latexMath
\sum_{i=1}^{n} a[i]
]]]

"
Class {
	#name : #PRCodeblockStylerExtensions,
	#superclass : #Object,
	#classInstVars : [
		'imageCache'
	],
	#category : #'Pillar-ExporterRichText-Core'
}

{ #category : #private }
PRCodeblockStylerExtensions class >> imageCache [
	^ imageCache ifNil: [ imageCache := LRUCache new ]
]

{ #category : #examples }
PRCodeblockStylerExtensions class >> latexExample [
	<example>
	(PRRichTextComposer  asText:
'!!!Latex mathematics 
It is possible to render latex math:
[[[language=latexMath
V_i = C_0 - C_3
\frac{C_1\cos(\theta_i+C_3)}{C_4+C_1\cos(\theta_i+C_2)}
]]]
Cool eh?
') inspect
]

{ #category : #styling }
PRCodeblockStylerExtensions class >> latexMathStylerFor: aString [
	<codeblockStylerFor:'latexMath'>
	| expressionUrl |
	expressionUrl := 'https://latex.codecogs.com/png.latex?', aString urlEncoded.
	^self loadExternalImageFromURL: expressionUrl errorText: 'Latex error'.
]

{ #category : #private }
PRCodeblockStylerExtensions class >> loadExternalImageFromURL: aUrl errorText: errorDesc [
	"I return a Text instance with an loaded from a url"
	| anchoredImage|
	anchoredImage := self imageCache 
		at: aUrl 
		ifAbsentPut: [ 
			[ anchoredImage := (String value: 1) asText
				addAttribute: (TextAnchor new anchoredMorph: (ZnEasy getPng: aUrl)).
				anchoredImage ] 
			on: Exception 
			do: [errorDesc  asText addAttribute: (TextColor red)]
		].
	^ anchoredImage 
]
