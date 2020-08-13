Extension { #name : #PRRichTextComposer }

{ #category : #'*Microdown-RichTextComposer' }
PRRichTextComposer class >> exampleMicrodown1 [
	<example>
	(self microdownAsRichText: '### First section
and this is just *first* line 
1. Line 1 More here More here ==More here More here== More here More here More here More here More here More here
   - Kasper tries the strange thing Kasper tries the strange thingKasper tries the strange thingKasper tries the strange thingKasper tries the strange thingKasper tries the strange thingKasper tries the strange thing
   - And one more __silly__ thingie
1. Line 2
   - Bla Bla Bla Bla Bla Bla Bla Bla Bla Bla Bla Bla Bla Bla Bla Bla Bla Bla Bla Bla Bla Bla Bla Bla Bla Bla Bla Bla Bla Bla Bla Bla Bla Bla Bla Bla
   - Bla Bla Bla Bla Bla Bla Bla Bla Bla Bla Bla Bla Bla Bla Bla Bla Bla Bla Bla Bla
1. Line 3') inspect
]

{ #category : #'*Microdown-RichTextComposer' }
PRRichTextComposer class >> exampleMicrodown11 [
	<example>
	(self microdownAsRichText:
'###Latex mathematics 
It is possible to render latex math via a web services. Future version will cache the resulting expressions. 

$
V_i = C_0 - C_3
\frac{C_1\cos(\theta_i+C_3)}{C_4+C_1\cos(\theta_i+C_2)}
$

Cool eh?
') inspect
]

{ #category : #'*Microdown-RichTextComposer' }
PRRichTextComposer class >> exampleMicrodown3 [
	<example>
	(self microdownAsRichText:
					'## Code Blocks
this is just _first_ line

```
Metacello new
   baseline: ''PillarRichTextExporter'';
   repository: ''github://kasperosterbye/PillarRichTextRender'';
   load.
```') inspect


]

{ #category : #'*Microdown-RichTextComposer' }
PRRichTextComposer class >> exampleMicrodown4 [
	<example>
	(self microdownAsRichText:
					'##Annotations
this is just _first_ line

!!todo remember to do annotations
and this is the second line') inspect
]

{ #category : #'*Microdown-RichTextComposer' }
PRRichTextComposer class >> exampleMicrodown5 [
	<example>
	(self microdownAsRichText:
					'###Links
This is just first line with an inline web ref [Pharo](http://pharo.org/)

[Click me to go to http://pharo.org](http://pharo.org/)

Here are direct links to classes and method
- `String`
- `String>>#trim`

And this is another line') inspect
]

{ #category : #'*Microdown-RichTextComposer' }
PRRichTextComposer class >> exampleMicrodown6 [
	<example>
	(self microdownAsRichText:
					'###Figures
This is just **first** line. Figures accessed via HTTP requests are cached for speed reasons. 
At max you should get one request per picture.

![Pharologo](https://files.pharo.org/media/logo/logo.png)

And this is the last line') inspect
]

{ #category : #'*Microdown-RichTextComposer' }
PRRichTextComposer class >> exampleMicrodown7 [
	<example>
	(self microdownAsRichText:
					'#First section

1. `Point` should open a class browser on Point.
2. `Point class` should open a class browser on Point.
3. `Point >> setX:setY:` should open a browser on the correspoding method.
4. `#''Pillar-ExporterRichText''` should open a browser on the package.
5. But `Borrrrrken` does not display the same.
') inspect
]

{ #category : #'*Microdown-RichTextComposer' }
PRRichTextComposer class >> exampleMicrodown8 [
	<example>
	(self microdownAsRichText:
					'```language=PharoExpression
1 + 2 * 3  
```
') inspect
]

{ #category : #'*Microdown-RichTextComposer' }
PRRichTextComposer class >> exampleMicrodown9 [
	<example>
	(self microdownAsRichText:
'In future versions, we should introduce adapted colorizer for expressions and for method bodies.
```language=PharoMethod
foo: arg
	^ arg + self
```
') inspect
]

{ #category : #'*Microdown-RichTextComposer' }
PRRichTextComposer class >> microdown [
	^ MicroDownParser 
	
]

{ #category : #'*Microdown-RichTextComposer' }
PRRichTextComposer class >> microdownAsDocument: aString [
	^ (self microdown parse: aString ) asPillar
]

{ #category : #'*Microdown-RichTextComposer' }
PRRichTextComposer class >> microdownAsRichText: aString [
	^ (self new start: (self microdown parse: aString ) asPillar)
]

{ #category : #'*Microdown-RichTextComposer' }
PRRichTextComposer class >> renderComment: aString of: aClassOrPackage [ 
	"Return aString as part of the templated class comment, when rendering is on.
	Else aString."
	
	| builder |
	builder := MicroDownParser builder.
	aClassOrPackage buildMicroDownUsing: builder withComment: aString.
	self notRendering 	
		ifTrue: [ ^ aString ].
	
	^ [ MicroDownParser asText: builder contents ] on: MicParsingError, MessageNotUnderstood, SubclassResponsibility  do: [ builder contents  ]
]
