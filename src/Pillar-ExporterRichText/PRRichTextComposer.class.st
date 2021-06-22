"
I am a visitor that produces rich text (class Text) based on a pillar document model.
I am implemented using a canvas/brush pattern. The class offers multiple configurations: an offline mode, 
a not rendering mode, and a not catching exceptions (to ease debugging).

The main API is the method: `PRRichTextComposer class>>#renderComment:of:`.

Web-based images or LaTeX rendering requires network access and are cached.



!!Note The composer should be improved to render better and in nicer ways elements. 

#### Advanced  
For the LaTeX rendering, internet connexion is required. 
You can also turn it off/on doing

```
PRRichTextComposer putOffLine
```

In addition, images fetched via http requests are cached to avoid multiple http requests.
The cache can be reset executing `PRRichTextComposer resetCache`. When the system is off line and an image is requested, a placeholder is computed but not put in the cache. This way when the system is put online the requested elements will be requested and put in the cache (without needing to flush placeholders from the cache).
When the system is online but the request failed I display an uncached placeholder.

It means that web-based images can be prefilled and Pharo images can be provided preloaded. 

When you want to get all the errors (typically to debug and fix them), you can configure the system to stop capturing errors using 

```
PRRichTextComposer captureErrors: false

```



#### Known limits
Monospaced text (e.g., `Point`, `Point class`,  `Point>>#setX:setY:`, or `#'Pillar-ExporterPillar'`) uses TextAction to trigger browser opening on the corresponding element. It would be better to use a TextLink because the styler is aware of them. 


"
Class {
	#name : #PRRichTextComposer,
	#superclass : #PRVisitor,
	#instVars : [
		'canvas',
		'monospaceBackgroundColor',
		'superscriptColor',
		'subscriptColor',
		'annotatedParagraphColor',
		'annotatedParagraphAnnotationColor',
		'annotatedParagraphAlignment'
	],
	#classVars : [
		'CaptureErrors',
		'HeaderFontSizes',
		'HeaderFonts',
		'ImageCache',
		'NotRendering',
		'OffLine',
		'Stylers'
	],
	#category : #'Pillar-ExporterRichText-Core'
}

{ #category : #private }
PRRichTextComposer class >> browse: urlString [
	"urlString is of the form: browse://Class or browse://Class>>#method"
	"self browse: 'browse://Class'"
	"self browse: 'browse://Point>>#x'"
	| parts sem |
	parts := urlString substrings: '/'.  
	sem := PRSemanticAction from: parts second.
	sem hasEntity 
		ifTrue: [ sem entity browse ].
	

]

{ #category : #private }
PRRichTextComposer class >> bulletForLevel: level [
	| kind |
	kind := level % 2.
	kind = 0
		ifTrue: [ ^ $• asText ].
	kind = 1
		ifTrue: [ ^ $- asText ]
]

{ #category : #'class initialization' }
PRRichTextComposer class >> captureErrors [
	
	^ CaptureErrors
]

{ #category : #'class initialization' }
PRRichTextComposer class >> captureErrors: aBoolean [
	
	CaptureErrors := aBoolean
]

{ #category : #'styler - declaration' }
PRRichTextComposer class >> codeStylerFor: aString [
	<codeblockStylerFor: 'Smalltalk'>
	
	^SHRBTextStyler new 
		isForWorkspace: true; 
		styledTextFor: aString asText
]

{ #category : #private }
PRRichTextComposer class >> computeHeaderFonts [
	
	HeaderFonts := Array new:  HeaderFontSizes size.
	1 to: HeaderFontSizes size do: [ :index |
		HeaderFonts at: index put: 
			(TextFontReference
				toFont:
					(StrikeFont
						familyName: 'Bitmap Source Sans Pro'
						pointSize: (HeaderFontSizes at: index)))
			  ]
]

{ #category : #private }
PRRichTextComposer class >> counterFor: counter atLevel: level [
	| kind |
	kind := level % 3.
	kind = 0
		ifTrue: [ ^ counter asString asText , '.' ].
	kind = 1
		ifTrue: [ ^ ($a asInteger + (counter - 1)) asCharacter asText , ')' ].
	kind = 2
		ifTrue: [ ^ ($A asInteger + (counter - 1)) asCharacter asText , ')' ]
]

{ #category : #'styler - declaration' }
PRRichTextComposer class >> defaultStyler: aString [
	<codeblockStylerFor: 'default'>
	"Pay attention without this one the logic can loop."
	
	^ self codeStylerFor: aString
]

{ #category : #'styler - declaration' }
PRRichTextComposer class >> expressionStylerFor: aString [
	<codeblockStylerFor: 'PharoExpression'>
	
	^SHRBTextStyler new 
		isForWorkspace: true; 
		styledTextFor: aString asText
]

{ #category : #private }
PRRichTextComposer class >> headerFontSizes: anArray [

	HeaderFontSizes := anArray.
	self computeHeaderFonts 
]

{ #category : #private }
PRRichTextComposer class >> headerFonts: anArray [

	HeaderFontSizes := anArray
]

{ #category : #private }
PRRichTextComposer class >> headerLevelFont: level [
	"I return a font annotation to be used with Attributes"

	^ HeaderFonts at: level
]

{ #category : #'class initialization' }
PRRichTextComposer class >> imageAt: urlString put: aForm [	
	"the cache contains results of for example ZnEasy getPng:, i.e., a form"
	
	ImageCache at: urlString put: aForm
]

{ #category : #'class initialization' }
PRRichTextComposer class >> initialize [ 
	"self initialize"
	self offline: false.
	self notRendering: false.
	self resetCache.
	self headerFontSizes: #(18 14 12 11 10 10).
	self initializeStylers.
	self captureErrors: true.
]

{ #category : #'class initialization' }
PRRichTextComposer class >> initializeStylers [
	<script>
	Stylers := Dictionary
		newFromAssociations: ((Pragma allNamed: #codeblockStylerFor:) 
		collect: [ :p | p arguments first asLowercase-> p method ])
]

{ #category : #stylers }
PRRichTextComposer class >> languageStylerFor: aLanguage [

	^ Stylers
		at: aLanguage asLowercase
		ifAbsent: [ self languageStylerFor: 'default' ]
]

{ #category : #'styler - declaration' }
PRRichTextComposer class >> methodStylerFor: aString [
	<codeblockStylerFor:'PharoMethod'>
	
	"when we use a pharo method for an expression the styler breaks for method pattern. So it should be fixed in the future but for now we make sure that at least it does not breaks."
	^[ SHRBTextStyler new 
		isForWorkspace: false; 
		styledTextFor: aString asText ] on: SubscriptOutOfBounds do: [ aString asText ]
]

{ #category : #'class initialization' }
PRRichTextComposer class >> notRendering [
	"When notRendering, the comments are just displayed as plain text."
	
	^ NotRendering
]

{ #category : #'class initialization' }
PRRichTextComposer class >> notRendering: aBoolean [
	NotRendering := aBoolean
]

{ #category : #'class initialization' }
PRRichTextComposer class >> offline [

	^ OffLine
]

{ #category : #'class initialization' }
PRRichTextComposer class >> offline: aBoolean [

	OffLine := aBoolean

]

{ #category : #settings }
PRRichTextComposer class >> preferencesSettingsOn: aBuilder [
	<systemsettings>
	(aBuilder group: #comments)
		label: 'Comment rendering';  
		parent: #appearance;
		description: 'All settings concerned with the notifications popup look''n feel';
		with: [
		(aBuilder setting: #offline)
			label: 'Enable offline mode';
			target: self;
			default: false;
			description: '
			When online is chosen, requests to external images are only performed when they are not in the image cache (since requesting systematically HTTP may lead of latency. Each figure is cached, so the request is at most done one per external references. When the system is online but the request failed we return a placeholder that is not cached.
			
			When offline, requests to external images are not performed. The cache can be reset executing PRRichTextComposer resetCache. When the system is off line and an image is requested, a placeholder is computed but not put in the cache. This way when the system is put online the requested elements will be requested and put in the cache (without needing to flush placeholders from the cache).'. 
			(aBuilder setting: #notRendering)
				label: 'Disable richtext rendering';
				default: false;
				target: self;
				description: 'Comments are by default rendered via Pillar in Richtext. When this setting is on, they are rendered as plain text (showing their Microdown definitions).'. 
			
			
			]
]

{ #category : #'class initialization' }
PRRichTextComposer class >> putOffline [

	self offline: true
]

{ #category : #'class initialization' }
PRRichTextComposer class >> putOnline [

	self offline: false
]

{ #category : #'class initialization' }
PRRichTextComposer class >> resetCache [

	ImageCache := Dictionary new
]

{ #category : #cleanup }
PRRichTextComposer class >> restartMethods [
    self initializeStylers
]

{ #category : #stylers }
PRRichTextComposer class >> style: aString asLanguage: aLanguage [
	"render aString according to the styler named aLanguage"
	
	| styler |
	styler := self languageStylerFor: aLanguage asLowercase.
	^ styler
		valueWithReceiver: styler methodClass soleInstance 
		arguments: (Array with: aString)
]

{ #category : #'styler - declaration' }
PRRichTextComposer class >> textStyler: aString [
	<codeblockStylerFor: 'text'>
	
	^aString asText
]

{ #category : #accessing }
PRRichTextComposer >> annotatedParagrapAlignment [
	^ annotatedParagraphAlignment 
]

{ #category : #accessing }
PRRichTextComposer >> annotatedParagraphAnnotationColor [
	^ annotatedParagraphAnnotationColor 
]

{ #category : #accessing }
PRRichTextComposer >> annotatedParagraphAnnotationColor: aColor [
	annotatedParagraphAnnotationColor := aColor
]

{ #category : #accessing }
PRRichTextComposer >> annotatedParagraphColor [
	^ annotatedParagraphColor 
]

{ #category : #accessing }
PRRichTextComposer >> annotatedParagraphColor: aColor [
	annotatedParagraphColor := aColor
]

{ #category : #images }
PRRichTextComposer >> cachedImageFromUrl: aUrl [
	
	^ OffLine 
		ifFalse: [
			ImageCache 
				at: aUrl asString
				ifAbsentPut: [ self rawImageFrom: aUrl ] ] 
		ifTrue: [ self imageOffLineFromUrl: aUrl ]
]

{ #category : #initialization }
PRRichTextComposer >> defaultAnnotatedParagraphColor [
	^ TextColor blue
]

{ #category : #initialization }
PRRichTextComposer >> defaultSuperscriptColor [
	^ TextColor blue
]

{ #category : #'images internal' }
PRRichTextComposer >> imageOffLineFromUrl: aUrl [
	"we are off but the image is there so we return it,"
	^ ImageCache 
			at: aUrl asString
			ifPresent: [ :i | i ]
			ifAbsent: [ (StringMorph contents: 'The HTTP request got unsuccessful. 
When you will be online, you should see: ' , aUrl asString) imageForm  ]
			
]

{ #category : #'images internal' }
PRRichTextComposer >> imageUnexpectedContentFromUrl: aUrl [
	"The request got unexpected content if the image is there so we return it. We do not cache the failing case image form so that we do not polute the cache."
	
	^ ImageCache 
			at: aUrl asString
			ifPresent: [ :i | i ]
			ifAbsent: [ (StringMorph contents: 'The HTTP request ', aUrl asString, ' returned an unexpected content.' ) imageForm  ]
			
]

{ #category : #initialization }
PRRichTextComposer >> initialize [
	super initialize.
	monospaceBackgroundColor := self theme settings windowColor.
	superscriptColor := self defaultSuperscriptColor.
	subscriptColor := TextColor red.
	annotatedParagraphColor := self defaultAnnotatedParagraphColor.
	annotatedParagraphAnnotationColor := TextEmphasis bold.
	annotatedParagraphAlignment := TextAlignment leftFlush
]

{ #category : #accessing }
PRRichTextComposer >> monospaceBackgroundColor [

	^ monospaceBackgroundColor
]

{ #category : #accessing }
PRRichTextComposer >> monospaceBackgroundColor: aColor [

	monospaceBackgroundColor := aColor
]

{ #category : #'images internal' }
PRRichTextComposer >> rawImageFrom: aURL [
	| block |
	self flag: #fixMeOnceNewExceptionIsAvailable.
	block := [ ZnEasy getImageOfType: (ZnMimeType main: 'image' sub: '*') fromUrl: aURL ].
	self class captureErrors
		ifFalse: [ ^ block value ].

	^ [ block
		on: ZnHttpUnsuccessful 
		do: [ :ex | self unsuccessfulRequestFromUrl: aURL ]]
			on: ZnUnexpectedContentType 
			do: [ :ex | self imageUnexpectedContentFromUrl: aURL ]
		
]

{ #category : #'visiting - document' }
PRRichTextComposer >> renderMathExpression: aString [
	
	canvas << (PRCodeblockStylerExtensions latexMathStylerFor: aString)
]

{ #category : #visiting }
PRRichTextComposer >> start: aDocument [
	canvas := PRRichTextCanvas new.
	self visit: aDocument.
	^ canvas contents
]

{ #category : #accessing }
PRRichTextComposer >> subscriptColor [
	^ subscriptColor 
]

{ #category : #accessing }
PRRichTextComposer >> subscriptColor: anObject [
	subscriptColor := anObject
]

{ #category : #accessing }
PRRichTextComposer >> superscriptColor [
	^ superscriptColor 
]

{ #category : #accessing }
PRRichTextComposer >> superscriptColor: anObject [
	superscriptColor := anObject
]

{ #category : #initialization }
PRRichTextComposer >> theme [

	^  Smalltalk ui theme
]

{ #category : #'images internal' }
PRRichTextComposer >> unsuccessfulRequestFromUrl: aUrl [
	
	^ ImageCache 
			at: aUrl asString
			ifPresent: [ :i | i ]
			ifAbsent: [ (StringMorph contents: 'The HTTP request ',  aUrl asString,  ' got unsuccessful.') imageForm  ]
			
]

{ #category : #visiting }
PRRichTextComposer >> visit: anObject [
	anObject accept: self
]

{ #category : #'visiting - unsupported' }
PRRichTextComposer >> visitAnchor: anObject [
	canvas << '>>> Anchors not supported in RichText Pillar <<<' asText
]

{ #category : #'visiting - document' }
PRRichTextComposer >> visitAnnotatedParagraph: anAnnotatedParagraph [
	
	canvas newLineIfNotAlready.
	canvas newLine.
	canvas
		includeAttribute: self annotatedParagrapAlignment
		in: [ 
			canvas
				includeAttribute: self annotatedParagraphAnnotationColor
				in: [ canvas << anAnnotatedParagraph annotation capitalized asText ].
			canvas
				includeAttribute: self annotatedParagrapAlignment
				in: [ canvas
						includeAttribute: self annotatedParagraphColor
						in: [ self visitParagraph: anAnnotatedParagraph ] ] ].
	canvas newLine.
]

{ #category : #'visiting - unsupported' }
PRRichTextComposer >> visitAnnotation: anAnnotation [
	canvas << '>>> Annotations not supported in RichText Pillar <<<' asText
]

{ #category : #'visiting - unsupported' }
PRRichTextComposer >> visitBeginEnvironmentAnnotation: aBeginEnvironmentAnnotation [

	canvas << '>>> Environments not supported in RichText Pillar <<<' asText
]

{ #category : #'visiting - format' }
PRRichTextComposer >> visitBoldFormat: anObject [
	canvas 
		includeAttribute: TextEmphasis bold 
		in: [self visitFormat: anObject]
	
]

{ #category : #'visiting - unsupported' }
PRRichTextComposer >> visitCitation: aCitation [
	canvas << '>>> Citations not supported in RichText Pillar <<<' asText
]

{ #category : #'visiting - unsupported' }
PRRichTextComposer >> visitCitationAnnotation: aCitation [
	canvas << '>>> Citations not supported in RichText Pillar <<<' asText
]

{ #category : #'visiting - document' }
PRRichTextComposer >> visitCodeblock: aCodeBlock [
	canvas newLineIfNotAlready.
	canvas newLine.
	canvas
		indentIn: [  
			canvas
				indentIn: [ 
					canvas << (self class  
						style: aCodeBlock text 
						asLanguage: aCodeBlock language originalName)
			] ].
	canvas newLine.

]

{ #category : #'visiting - unsupported' }
PRRichTextComposer >> visitColumn: aColumn [
	canvas << '>>> Columns not supported in RichText Pillar <<<' asText
]

{ #category : #'visiting - unsupported' }
PRRichTextComposer >> visitColumnAnnotation: aColumnAnnotation [
	canvas << '>>> Columns not supported in RichText Pillar <<<' asText
]

{ #category : #'visiting - unsupported' }
PRRichTextComposer >> visitColumns: aColumns [
	canvas << '>>> Columns not supported in RichText Pillar <<<' asText
]

{ #category : #'visiting - unsupported' }
PRRichTextComposer >> visitColumnsAnnotation: aColumnsAnnotation [
	canvas << '>>> Columns not supported in RichText Pillar <<<' asText
]

{ #category : #'visiting - list' }
PRRichTextComposer >> visitDataItem: anItem [
	canvas newLineIfNotAlready.
	self visitDocumentGroup: anItem
]

{ #category : #'visiting - list' }
PRRichTextComposer >> visitDefinitionList: anObject [
	canvas indentIn: [ self visitUnorderedList: anObject ]
]

{ #category : #'visiting - unsupported' }
PRRichTextComposer >> visitDocListAnnotation: aPRDocListAnnotation [
	canvas << '>>> DocLists not supported in RichText Pillar <<<' asText
]

{ #category : #'visiting - document' }
PRRichTextComposer >> visitEmptyParagraph: anObject [
	canvas newLine
]

{ #category : #'visiting - unsupported' }
PRRichTextComposer >> visitEndColumnsAnnotation: aEndColumnsAnnotation [
	canvas << '>>> Columns not supported in RichText Pillar <<<' asText
]

{ #category : #'visiting - unsupported' }
PRRichTextComposer >> visitEndEnvironmentAnnotation: aEndEnvironmentAnnotation [
	canvas << '>>> Environments not supported in RichText Pillar <<<' asText
]

{ #category : #'visiting - document' }
PRRichTextComposer >> visitEnvironment: anObject [
	self flag: #fixme.
	"What is this textBlock idea? Clean!"
	anObject name = 'textBlock'
		ifTrue:[
			canvas indentIn: [  
				canvas 
					includeAttribute: TextEmphasis italic  
					in: [self visitFormat: anObject]] ]
		ifFalse: [canvas << ('>>> Environment ' , anObject name , 
						'is not supported in RichText Pillar <<<') asText]
]

{ #category : #'visiting - document' }
PRRichTextComposer >> visitExternalLink: aLink [
	| attribute target url |
	target := aLink reference.
	url := target asUrl.
	attribute := nil.
	(#(http https) includes: url scheme )
		ifTrue: [ attribute := "PRExternalLinkTextAction url: target"
			 	TextAction new actOnClickBlock: [WebBrowser openOn: target]].
	url scheme = #browse
		ifTrue:
			[ attribute := TextAction new actOnClickBlock: [ self class browse: target ] ].
	attribute
		ifNotNil:
			[ canvas includeAttribute: attribute in: [ self visitLink: aLink ] ]
		ifNil: [ self visitLink: aLink ]
]

{ #category : #'visiting - document' }
PRRichTextComposer >> visitFigure: aFigure [
	| anchoredImage |
	"Try to read a png file (only one supported right now), else use alt text"
	
	"for now I cancel the catching of all exceptions because it is a blackhole."
	"[" 
	anchoredImage := (String value: 1) asText
		addAttribute: (TextAnchor new anchoredMorph: (self cachedImageFromUrl: aFigure reference)).
		canvas << anchoredImage 
		"] 
		on: Exception  
		do: [self visitAll: aFigure children]."
]

{ #category : #'visiting - document' }
PRRichTextComposer >> visitHeader: aHeader [

	canvas newLineIfNotAlready.
	canvas newLine.
	canvas
		includeAttribute: (self class headerLevelFont: aHeader level)
		in: [ self visitDocumentGroup: aHeader ].
	canvas newLine
]

{ #category : #'visiting - document' }
PRRichTextComposer >> visitHorizontalRule: anHorizontalLine [

	canvas includeAttribute: TextEmphasis underlined in: [ canvas << ('     ' repeat: 20) asText ].
	canvas newLine.	
]

{ #category : #'visiting - format' }
PRRichTextComposer >> visitItalicFormat: anObject [
	canvas includeAttribute: TextEmphasis italic in: [ self visitFormat: anObject ]
]

{ #category : #'visiting - list' }
PRRichTextComposer >> visitListItem: anItem [
	| bullet |
	"I stored the counter and bullet type in `visitOrderedList` and `visitUnorderedList`"
	bullet := (anItem propertyAt: #kind) = #ordered
		ifTrue: [ (anItem propertyAt: #counter) asString asText , String tab ]
		ifFalse: [ (anItem propertyAt: #bullet) asText , String tab ].
	bullet addAttribute: (TextIndent tabs: canvas nesting - 1).
	canvas newLineIfNotAlready.
	canvas << bullet.
	self visitDocumentGroup: anItem.
	"canvas newLineIfNotAlready."
]

{ #category : #'visiting - document' }
PRRichTextComposer >> visitMathEnvironment:  aPRMathEnvironment [
	"we could plug some math rendering for Polymath following the implementation of Kasper.
	See PRCodeblockStylerExtensions"
	| text |
	text := aPRMathEnvironment text. 
	(OffLine | text isNil)
		ifTrue: [ text 
						ifNotNil: [ canvas << '&'; cr; << text; << '&']
						ifNil: [ canvas << '&'; cr; << 'Your math environment is strange its body is nil'; << '&' ] ]
		ifFalse: [ canvas cr.
					canvas indentIn: [ self renderMathExpression: text ].
					 canvas cr ]
]

{ #category : #'visiting - document' }
PRRichTextComposer >> visitMathInTextAnnotation: aPRMathInTextAnnotation [ 
	"we could plug some math rendering for Polymath following the implementation of Kasper.
	See PRCodeblockStylerExtensions"
	
	OffLine 
		ifTrue: [ canvas << '$', aPRMathInTextAnnotation text, '$' ]
		ifFalse: [ canvas << (PRCodeblockStylerExtensions latexMathStylerFor: aPRMathInTextAnnotation text)]
]

{ #category : #'visiting - format' }
PRRichTextComposer >> visitMonospaceFormat: anObject [
	"Notice - I add a highlight to the backgorund rather than mono-spaced."
	| backgroundColor sem |
	backgroundColor := self monospaceBackgroundColor.
	
	self flag: #fixMe.
	"Right now we stupidly emit TextAction but we should emit TextClassLink and revisit them."
	canvas 
		includeAttribute: (TextBackgroundColor color: backgroundColor) 
		in: [ 
			sem := PRSemanticAction from: anObject text.
			sem hasEntity 
				ifTrue: [ canvas 
								includeAttribute: (TextAction new actOnClickBlock: [ sem entity browse ]) in: [self visitFormat: anObject ] ]
				ifFalse: [ self visitFormat: anObject]].  
]

{ #category : #'visiting - list' }
PRRichTextComposer >> visitOrderedList: anObject [
	| counter |
	canvas indentIn: [counter := 1.
		anObject children
			do: [ :item | 
				item propertyAt: #kind put: #ordered.
				item
					propertyAt: #counter
					put: (self class counterFor: counter atLevel: canvas nesting - 1).
				counter := counter + 1 ].
		super visitOrderedList: anObject ].
	canvas newLine
]

{ #category : #'visiting - document' }
PRRichTextComposer >> visitParagraph: anObject [
	"canvas newLineIfNotAlready."
	canvas newLine.
	super visitParagraph: anObject.
	canvas newLine.
]

{ #category : #'visiting - document' }
PRRichTextComposer >> visitParameters: anObject [
	"do nothing for now"
	
	^ self
]

{ #category : #'visiting - document' }
PRRichTextComposer >> visitPreformatted: aPreformatted [

	canvas newLineIfNotAlready.
	canvas newLine.
	canvas
		indentIn: [ 
			aPreformatted children do: [ :each | 
				canvas << (self class textStyler: each text).
				canvas newLine ].
			"This is not really correct since the text should be interpreted."
			"canvas << (PRRichTextCodeblockStyler textStyler: aPreformatted text)"].
		
	canvas newLine
]

{ #category : #'visiting - format' }
PRRichTextComposer >> visitStrikethroughFormat: anObject [
	canvas includeAttribute: TextEmphasis struckOut in: [ self visitFormat: anObject ]
]

{ #category : #'visiting - format' }
PRRichTextComposer >> visitSubscriptFormat: anObject [
	canvas
		includeAttribute: self subscriptColor
		in: [ self visitFormat: anObject ]
]

{ #category : #'visiting - format' }
PRRichTextComposer >> visitSuperscriptFormat: anObject [
	canvas
		includeAttribute: self superscriptColor
		in: [ self visitFormat: anObject ]
]

{ #category : #'visiting - list' }
PRRichTextComposer >> visitTermItem: anItem [
	canvas newLineIfNotAlready.
	canvas
		includeAttribute: (TextIndent tabs: canvas nesting - 1)
		in: [ canvas
				includeAttribute: TextEmphasis bold
				in: [ self visitDocumentGroup: anItem ] ]
]

{ #category : #'visiting - document' }
PRRichTextComposer >> visitText: anObject [
	canvas << anObject text asText
		" - should not be necessary addAttribute: (TextIndent tabs: canvas nesting)"
]

{ #category : #'visiting - format' }
PRRichTextComposer >> visitUnderlineFormat: anObject [
	canvas includeAttribute: TextEmphasis underlined in: [ self visitFormat: anObject ]
]

{ #category : #'visiting - list' }
PRRichTextComposer >> visitUnorderedList: anObject [
	canvas
		indentIn: [ anObject children
				do: [ :item | 
					item propertyAt: #kind put: #unordered.
					item
						propertyAt: #bullet
						put: (self class bulletForLevel: canvas nesting - 1) ].
			super visitUnorderedList: anObject ].
	canvas newLine
]
