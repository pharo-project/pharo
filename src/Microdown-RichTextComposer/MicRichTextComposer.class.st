"
I'm a visitor that produces rich text (class `Text`) based on a microdown model.
I'm implemented using a canvas/brush pattern. The class offers multiple configurations: an offline mode, 
a not rendering mode, and a not catching exceptions (to ease debugging).

The main API is the methods: `MicRichTextComposer class >> #renderComment:of:` and `MicRichTextComposer class >> #asRichText:`

Web-based images or LaTeX rendering requires network access and are cached.

#### Advanced  
For the LaTeX rendering, internet connexion is required. 
You can also turn it off/on doing

```
MicRichTextComposer putOffLine
```

In addition, images fetched via http requests are cached to avoid multiple http requests.
The cache can be reset executing `MicRichTextComposer resetCache`. When the system is off line and an image is requested, a placeholder is computed but not put in the cache. This way when the system is put online the requested elements will be requested and put in the cache (without needing to flush placeholders from the cache).
When the system is online but the request failed I display an uncached placeholder.

It means that web-based images can be prefilled and Pharo images can be provided preloaded. 

When you want to get all the errors (typically to debug and fix them), you can configure the system to stop capturing errors using 

```
MicRichTextComposer captureErrors: false

```



#### Known limits
Monospaced text (e.g., `Point`, `Point class`,  `Point>>#setX:setY:`, or `#'Pillar-ExporterPillar'`) uses TextAction to trigger browser opening on the corresponding element. It would be better to use a TextLink because the styler is aware of them. 


"
Class {
	#name : #MicRichTextComposer,
	#superclass : #MicrodownVisitor,
	#instVars : [
		'canvas',
		'textStyler',
		'codeStylerClass'
	],
	#category : #'Microdown-RichTextComposer-Composer'
}

{ #category : #public }
MicRichTextComposer class >> asRichText: aStringOrDoc [
	
	^ self new asRichText: aStringOrDoc

]

{ #category : #examples }
MicRichTextComposer class >> exampleMicrodown1 [
	<sampleInstance>
	^ self parseAndCompose:  '### Emphasis and lists
Paragraphs are seperated by blank lines. Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
New lines are preserved. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 

Pargraphs can be split over several lines.
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.
1. Lorem ipsum dolor sit amet, `consectetur` adipiscing elit, 
   sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
   - _Lorem_ **ipsum** dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor 
     incididunt ut labore et dolore magna aliqua.
   - Lorem _**ipsum dolor**_ sit amet
1. You indent by alligning the bullet pointer (-, *, + or number) to the beginning of the text in above
   - Like this
   - Or this
1. For the moment numbered lists always start with 1, and count upwards'
]

{ #category : #examples }
MicRichTextComposer class >> exampleMicrodown2 [
	<sampleInstance>
	^ self parseAndCompose:
'## Code Blocks

Code blocks can be Pharo, Method, or text. Default is Pharo

```Pharo
Metacello new
   baseline: ''PillarRichTextExporter'';
   repository: ''github://kasperosterbye/PillarRichTextRender'';
   load.
```

```Method
textStyler: aString
	<codeblockStyler: ''text''>
	^aString asText
```

```text
textStyler: aString
	<codeblockStyler: ''text''>
	^aString asText
```

' 


]

{ #category : #examples }
MicRichTextComposer class >> exampleMicrodown3 [
	<sampleInstance>
	^ self parseAndCompose:
					'### Links
This is just first line with an inline web ref [Pharo](http://pharo.org/)

[Click me to go to http://pharo.org](http://pharo.org/)

Here are direct links to classes and method
- `String`
- `String>>#trim`

And this is another line'
]

{ #category : #examples }
MicRichTextComposer class >> exampleMicrodown4 [
	<sampleInstance>
	^ self parseAndCompose:
					'### Figures
This is just **first** line. Figures accessed via HTTP requests are cached for speed reasons. 
You should get one request per picture.

![A super nice Pharo logo.](https://files.pharo.org/media/logo/logo.png)

Images can be used as anchors: 

[![](https://pharo.org/web/files/pharo-logo-small.png)](https://pharo.org)

'
]

{ #category : #examples }
MicRichTextComposer class >> exampleMicrodown5 [
	<sampleInstance>
	^ self parseAndCompose:
					'# Links to code

1. `Point` should open a class browser on Point.
2. `Point class` should open a class browser on Point.
3. `Point >> setX:setY:` should open a browser on the correspoding method.
4. `#''Kernel-BasicObjects''` should open a browser on the package.
5. But `Borrrrrken` does not create a link to code.
' 
]

{ #category : #examples }
MicRichTextComposer class >> exampleMicrodown6 [
	<sampleInstance>
	^ self parseAndCompose:
'### Latex mathematics 
It is possible to render latex math via a web services. We allow inline math $x^2$, or math blocks

&
 \frac{C_1\cos(\theta_i+C_3)}{C_4+C_1\cos(\theta_i+C_2)}
&

Cool eh?
'
]

{ #category : #examples }
MicRichTextComposer class >> exampleMicrodown7 [
	<sampleInstance>
	
	^ self parseAndCompose: '## This is the class Object
	
	
	![Object class.](pharo:///Object)'
]

{ #category : #examples }
MicRichTextComposer class >> exampleMicrodown8 [
	<sampleInstance>
	^ self parseAndCompose:
					'## Annotations
this is just _first_ line

!!todo remember to do annotations
and this is the second line'
]

{ #category : #'class initialization' }
MicRichTextComposer class >> initialize [ 
	"self initialize"
	MicCodeStylerSpec initializeCache.
]

{ #category : #public }
MicRichTextComposer class >> parseAndCompose: aString [
	
	^ self new visit: (Microdown parse: aString)

]

{ #category : #'visiting - document' }
MicRichTextComposer >> anchorFor: anImageOrMorph of: aNode [

	aNode arguments at: #width ifPresent: [ :width |
		(width endsWith: '%') 
			ifTrue: [ |scale|
				scale := width initialIntegerOrNil.
				scale ifNotNil: [ 
					^ MicScalingTextAnchor new original: anImageOrMorph; scale: scale; yourself
					 ]]
		   ifFalse:  [ 
				width initialIntegerOrNil 
					ifNotNil: [ :absWidth | |absHeight|
						absHeight := anImageOrMorph height / anImageOrMorph width *absWidth.
						anImageOrMorph isMorph 
							ifTrue: [ ^ TextAnchor new anchoredMorph: (anImageOrMorph extent: absWidth @ absHeight) ]
							ifFalse: [ ^ TextAnchor new anchoredMorph: (anImageOrMorph scaledToSize: absWidth @ absHeight) ]
						  ]
		 ]].
	^ TextAnchor new anchoredMorph: anImageOrMorph
]

{ #category : #public }
MicRichTextComposer >> asRichText: aStringOrDoc [
	
	
	^ self visit: (aStringOrDoc isString 
		ifTrue: [ Microdown parse: aStringOrDoc ]
		ifFalse: [ aStringOrDoc ])
]

{ #category : #accessing }
MicRichTextComposer >> codeStylerClass [

	^ codeStylerClass
]

{ #category : #initialization }
MicRichTextComposer >> initialize [
	super initialize.
	codeStylerClass := MicCodeStylerSpec.
	self textStyler:  MicTextStyler new.
]

{ #category : #private }
MicRichTextComposer >> latexFor: aString onError: aBlock [

	"Return a Text containing an image with the latex formula.
	If the latex formula cannot be built because of network or syntax errors,
	use the error block to build a text"

	| ref resource |
	ref := MicResourceReference fromUri: aString.
	resource := ref resolveResourceIn: (MicUrlResolver new baseUrlString: 'https://latex.codecogs.com/png.latex?').
	^ (resource asForm ifNil: aBlock) asText
]

{ #category : #composing }
MicRichTextComposer >> parseAndCompose: aString [ 
	
	^ self visit: (Microdown parse: aString)

]

{ #category : #public }
MicRichTextComposer >> parseAndComposeStringOrDoc: aStringOrDoc [
	
	^ self visit: (aStringOrDoc isString 
			ifTrue: [ Microdown parse: aStringOrDoc ]
			ifFalse: [ aStringOrDoc ])
]

{ #category : #'figure-resolution' }
MicRichTextComposer >> renderHttpResource: aMicHttpResource inNode: aNode [

	^ (String value: 1) asText
		addAttribute: (self anchorFor: aMicHttpResource asForm of: aNode);
		yourself
]

{ #category : #'figure-resolution' }
MicRichTextComposer >> renderNotFoundResource: aMicNotFoundResource inNode: aNode [

	^ aNode caption asText
]

{ #category : #'figure-resolution' }
MicRichTextComposer >> renderPharoClassResource: aMicPharoClassResource inNode: aNode [
	
	"Put a class as text inside the document"
	^ aMicPharoClassResource asText
]

{ #category : #private }
MicRichTextComposer >> renderTableCell: aCell [
	"a cell is an array of nodes. Each element should be rendered and concatenated"
	^ aCell inject: Text new into: [ :txt :part | 
		txt, (MicRichTextComposer new visit: part)
		 ]
]

{ #category : #'visiting - document' }
MicRichTextComposer >> resizeImage: anImage of: aFigure [

	| scaledImage |
	scaledImage := anImage.
	"aFigure arguments at: #width ifPresent: [ :s |
		hostMorph 
			ifNotNil: [
				scaledImage := anImage scaledToSize: ( s asNumber in: [ :scale | 
					(hostMorph width / 100 * scale) @ (hostMorph height / 100 * scale)]).
		 ]]."
	^ scaledImage
]

{ #category : #accessing }
MicRichTextComposer >> textStyler [

	^ textStyler
]

{ #category : #accessing }
MicRichTextComposer >> textStyler: aTextStyler [

	textStyler := aTextStyler.
]

{ #category : #initialization }
MicRichTextComposer >> theme [

	^  Smalltalk ui theme
]

{ #category : #accessing }
MicRichTextComposer >> todo [ 
	^ 'there is too much hspace after a section when it is followed by a paragraph. 
This also because people will not want to to not let a newline between the header and paragraph when writing the text.'
]

{ #category : #visiting }
MicRichTextComposer >> visit: aDocument [

	canvas := MicRichTextCanvas new
		          textStyler: self textStyler;
		          yourself.
	super visit: aDocument.
	^ canvas contents
]

{ #category : #'visiting - document' }
MicRichTextComposer >> visitAnnotatedParagraph: anAnnotatedParagraph [
	"Do nothing - You are supposed to use specialized annotations"
	self error: 'You are not supposed to be here - did you misspell an annotation type?'
	
]

{ #category : #'visiting -  format' }
MicRichTextComposer >> visitBold: anObject [
	canvas 
		includeAttribute: TextEmphasis bold 
		in: [ super visitBold: anObject ]
	
]

{ #category : #visiting }
MicRichTextComposer >> visitCode: aCodeBlock [

	canvas newLineIfNotAlready.
	canvas indentIn: [ 
		canvas
			<< (self codeStylerClass 
				style: aCodeBlock body 
				asLanguage: aCodeBlock language);
			newLine.
		aCodeBlock hasCaption ifTrue: [ 
			canvas 
				includeAttribute: TextEmphasis bold 
				in: [ 
					canvas
						<< aCodeBlock caption asText;
					newLine ]]].
	canvas << textStyler interBlockSpacing
		
]

{ #category : #'visiting -  format' }
MicRichTextComposer >> visitColumn: columnBlock [
	"I do nothing - handled by columns"
	
	
]

{ #category : #'visiting -  format' }
MicRichTextComposer >> visitColumns: columnsBlock [
	"I render the using MicRichTable which is dedicated to this. I insert the table as an image (form)"
	| columns columnsBlocks totalWidth|
	canvas newLineIfNotAlready.
	columnsBlocks := columnsBlock children select: [ :c | c isKindOf: MicColumnBlock].
	columns := columnsBlocks collect: [ :column | |colText|
		colText := Microdown asRichText: (MicRootBlock new children: column children;yourself).
		RubScrolledTextMorph new setText: colText].
	totalWidth := columns sum: [ :col | col textArea width].
	columns do: [ :morph |
		canvas << ((String value: 1) asText addAttribute: 
			(MicScalingTextAnchor new 
				original: morph;
				"There need to be a bit of extra space assigned (5%)"
				scale: (morph textArea width / totalWidth * 100)-5;
				"Need to add a bit of space (15) for the scrollbar to disappear"
				freezeHeight: morph textArea height+15))].
	canvas 
		newLine; 
		<< textStyler interBlockSpacing 
	
	
]

{ #category : #'visiting - document' }
MicRichTextComposer >> visitEnvironment: anObject [
	"Environment is an abstract class, and if we end here it is because of error"
	canvas << ('>>> Environment ' , anObject name , 
						'is not supported in RichText Pillar <<<') asText
]

{ #category : #'visiting - inline elements' }
MicRichTextComposer >> visitFigure: aFigure [

	| imageResource embeddableInText |
	imageResource := aFigure reference resolveResourceIn: MicUrlResolver new.
	embeddableInText := imageResource asRenderedOn: self withinNode: aFigure.
	"Adding a zero width no-break space circumvents two errors in rubrics when
	using images as links. Adding it even if the image is not a link causes no
	harm (it is invisible), and this is the easiest place to add it."
	canvas << (embeddableInText, 16rFEFF asCharacter asString asText)
]

{ #category : #'visiting - document' }
MicRichTextComposer >> visitHeader: aHeader [

	canvas newLineIfNotAlready.
	canvas
		includeAttribute: (self textStyler headerLevelFont: aHeader level)
		in: [ super visitHeader: aHeader ].
	canvas << (textStyler spaceAfterHeaderLevel: aHeader level)
]

{ #category : #'visiting - document' }
MicRichTextComposer >> visitHorizontalLine: anHorizontalLine [
	
	canvas newLineIfNotAlready.
	canvas includeAttribute: TextEmphasis underlined in: [ canvas << ('     ' repeat: 20) asText ].
	canvas newLine.	
]

{ #category : #'visiting -  format' }
MicRichTextComposer >> visitItalic: anObject [
	canvas 
		includeAttribute: TextEmphasis italic 
		in: [ super visitItalic: anObject ]
]

{ #category : #'visiting - document' }
MicRichTextComposer >> visitLink: aLink [

	| attribute |
	attribute := TextAction new
		             actOnClickBlock: [ 
			             WebBrowser openOn: aLink url ];
		             yourself.
	canvas includeAttribute: attribute in: [ super visitLink: aLink ]
]

{ #category : #'visiting - list' }
MicRichTextComposer >> visitListItem: anItem [
	| bullet |
	"The depth counter and bullet type in `visitOrderedList` and `visitUnorderedList`"
	bullet := (anItem propertyAt: #kind) = #ordered
		ifTrue: [ (anItem propertyAt: #counter) asString asText , String tab ]
		ifFalse: [ (anItem propertyAt: #bullet) asText , String tab ].
	bullet addAttribute: (TextIndent tabs: canvas nesting - 1).
	canvas newLineIfNotAlready.
	canvas << bullet.
	canvas indentIn: [ super visitListItem: anItem ].

]

{ #category : #'visiting - document' }
MicRichTextComposer >> visitMath:  aMicMathEnvironment [

	| aString |
	aString := aMicMathEnvironment body.
	aString ifEmpty: [ ^self ].
	canvas << (self latexFor: aString onError: ['& - LATEX ERROR - &' asText] )
]

{ #category : #'visiting - document' }
MicRichTextComposer >> visitMathInline: aMicMathInline [ 
	| aString  |
	aString := aMicMathInline substring .
   aString ifEmpty: [^ self].
	canvas << (self latexFor: aString onError: [ '$ - LATEX ERROR - $' asText ])
]

{ #category : #'visiting -  format' }
MicRichTextComposer >> visitMonospace: anObject [
	"Notice - I add a highlight to the backgorund rather than mono-spaced."
	| backgroundColor sem |
	backgroundColor := self textStyler monospaceBackgroundColor.
	self flag: #fixMe.
	"Right now we stupidly emit TextAction but we should emit TextClassLink and revisit them."
	canvas 
		includeAttribute: (TextBackgroundColor color: backgroundColor) 
		in: [ 
			sem := MicSemanticAction from: anObject text.
			sem hasEntity 
				ifTrue: [ canvas 
								includeAttribute: (TextAction new actOnClickBlock: [ sem entity browse ]) 
								in: [ super visitMonospace: anObject ] ]
				ifFalse: [ super visitMonospace: anObject]].  
]

{ #category : #'visiting - list' }
MicRichTextComposer >> visitOrderedList: anObject [
	"here we annotate items with their kind and the depth"
	| counter |
	canvas newLineIfNotAlready.
	canvas indentIn: [counter := 1.
		anObject children
			do: [ :item | 
				item propertyAt: #kind put: #ordered.
				item
					propertyAt: #counter
					put: (self textStyler counterFor: counter atLevel: canvas nesting - 1).
				counter := counter + 1 ].
		super visitOrderedList: anObject ].
	canvas newLine; << textStyler interBlockSpacing 
]

{ #category : #'visiting - document' }
MicRichTextComposer >> visitParagraph: anObject [
	
	canvas newLineIfNotAlready.
	super visitParagraph: anObject.
	canvas newLine; << textStyler interBlockSpacing 
]

{ #category : #'visiting - document' }
MicRichTextComposer >> visitParameters: anObject [
	"do nothing for now"
	
	^ self
]

{ #category : #visiting }
MicRichTextComposer >> visitQuote: aQuote [
	"I should have a fancier implementation, but for now this should work and be recognized as a quote"
	canvas newLineIfNotAlready.
	canvas << ('>',String tab) asText..
	canvas indentIn: [ super visitQuote: aQuote ].
	canvas newLine
]

{ #category : #'visiting -  format' }
MicRichTextComposer >> visitStrike: anObject [

	canvas 
		includeAttribute: TextEmphasis struckOut 
		in: [ super visitStrike: anObject ]
]

{ #category : #'visiting -  format' }
MicRichTextComposer >> visitTable: tableBlock [
	"I render the using MicRichTable which is dedicated to this. I insert the table as an image (form)"
	| headers rows table anchoredTable renderedRows |
	renderedRows := tableBlock rows collect: [ :oneRow | oneRow collect: [ :cell | self renderTableCell: cell ]].
	tableBlock hasHeader
		ifTrue: [ 
			headers := renderedRows first. 
			rows := renderedRows allButFirst  ]
		ifFalse: [ 
			headers := (1 to: renderedRows first size) collect: [:i | i printString asText].
			rows := renderedRows].
	"Create the Morphic Table and insert it"
	table := MicRichTextTable headers: headers rows: rows.
	tableBlock hasHeader
		ifFalse: [ 
			table extent: table extent - (0 @ '1' asTextMorph height). "not showing the headers"
			table hideColumnHeaders  ].
	anchoredTable := (String value: 1) asText addAttribute: (TextAnchor new anchoredMorph: table).
	canvas newLineIfNotAlready;newLine.
	canvas 
		<< anchoredTable;
		newLine;
		<< textStyler interBlockSpacing 
	
]

{ #category : #'visiting -  format' }
MicRichTextComposer >> visitText: anInlineBlock [
	"we should set attribute because it would override link and others."
	canvas << (textStyler postTextTreatment: (anInlineBlock substring asText) )
]

{ #category : #'visiting - list' }
MicRichTextComposer >> visitUnorderedList: anObject [
	canvas newLineIfNotAlready.
	canvas
		indentIn: [ 
			anObject children
				do: [ :item | 
					item propertyAt: #kind put: #unordered.
					item
						propertyAt: #bullet
						put: (self textStyler bulletForLevel: canvas nesting - 1) ].
			super visitUnorderedList: anObject ].
	canvas newLine; << textStyler interBlockSpacing 
]
