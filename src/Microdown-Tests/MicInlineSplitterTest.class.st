"
This class contains tests about the new approach to identify inline elements inside blocks.
"
Class {
	#name : #MicInlineSplitterTest,
	#superclass : #TestCase,
	#category : #'Microdown-Tests'
}

{ #category : #accessing }
MicInlineSplitterTest >> splitter [
	^ MicInlineSplitter new
]

{ #category : #tests }
MicInlineSplitterTest >> testAnchorReferenceUnevaluated [
	"When isEvaluated class method returns false, like anchor reference's case, inline inside shoudn't be evaluated"
	| res |
	res := self splitter start: 'abc*@def**not bold**ghi@*xyz'.
	self
		assert: (res collect: [ :x | x printString ])
		equals: {'abc' . '[def**not bold**ghi](anchorReference)' . 'xyz'}
]

{ #category : #'tests - escape' }
MicInlineSplitterTest >> testEscapeCharacter [
	"Test the escape \ in simple case (here, bold one)"

	| res |
	res := self splitter start: 'abc\**test**last'.
	self
		assert: (res collect: [ :x | x printString ])
		equals: {'abc**test**last'}.
]

{ #category : #'tests - escape' }
MicInlineSplitterTest >> testEscapeCharacterAtBeginning [
	"Test the escape \ in simple case (here, bold one)"

	| res |
	res := self splitter start: '\**test**'.
	self
		assert: (res collect: [ :x | x printString ])
		equals: {'**test**'}.
]

{ #category : #'tests - escape' }
MicInlineSplitterTest >> testEscapeCharacterInLinkName [
	"Test the escape \ in link description"

	| res |
	res := self splitter start: 'abc[\**test**](myURL)last'.
	self
		assert: (res collect: [ :x | x printString ])
		equals: {'abc' . '[**test**](linkName)' . 'last'}.
]

{ #category : #'tests - escape' }
MicInlineSplitterTest >> testEscapeCharacterInNestedCase [
	"Test the escape \ in case of nested formats"

	| res |
	res := self splitter start: 'abc_\**not bold** italic_last'.
	self
		assert: (res collect: [ :x | x printString ])
		equals: {'abc' . '[**not bold** italic](italic)' . 'last'}.
]

{ #category : #'tests - escape' }
MicInlineSplitterTest >> testEscapeCharacterInNestedLinkName [
	"Test the escape \ in case of nested linknames"

	| res |
	res := self splitter start: 'abc[\[nested\]](myURL)last'.
	self
		assert: (res collect: [ :x | x printString ])
		equals: {'abc' . '[[nested]](linkName)' . 'last'}.
]

{ #category : #'tests - monospace' }
MicInlineSplitterTest >> testEscapeMonospace [
	"When isEvaluated class method returns false, like monospace's case, inline inside shoudn't be evaluated"
	| res |
	res := self splitter start: 'abc`\`monospace\``efg'.
	self
		assert: (res collect: [ :x | x printString ])
		equals: {'abc' . '[`monospace`](monospace)' . 'efg'}
]

{ #category : #tests }
MicInlineSplitterTest >> testMathUnevaluated [
	"When isEvaluated class method returns false, like math's case, inline inside shoudn't be evaluated"
	| res |
	res := self splitter start: 'abc$def**not bold**ghi$xyz'.
	self
		assert: (res collect: [ :x | x printString ])
		equals: {'abc' . '[def**not bold**ghi](math)' . 'xyz'}
]

{ #category : #'tests - monospace' }
MicInlineSplitterTest >> testMonospaceUnevaluated [
	"When isEvaluated class method returns false, like monospace's case, inline inside shoudn't be evaluated"
	| res |
	res := self splitter start: 'abc`def**not bold**ghi`xyz'.
	self
		assert: (res collect: [ :x | x printString ])
		equals: {'abc' . '[def**not bold**ghi](monospace)' . 'xyz'}
]

{ #category : #tests }
MicInlineSplitterTest >> testRawUnevaluated [
	"When isEvaluated class method returns false, like raw's case, inline inside shoudn't be evaluated"
	| res |
	res := self splitter start: 'abc{{def**not bold**ghi}}xyz'.
	self
		assert: (res collect: [ :x | x printString ])
		equals: {'abc' . '[def**not bold**ghi](raw)' . 'xyz'}
]

{ #category : #tests }
MicInlineSplitterTest >> testSplitAnnotation [
	| res |
	res := self splitter start: 'abc<?type:value|key1=val1&key2=val2?>def'.
	self
		assert: (res collect: [ :x | x printString ])
		equals: {'abc' . '[type:value|key1=val1&key2=val2](annotation)' . 'def'}.
]

{ #category : #tests }
MicInlineSplitterTest >> testSplitBlock2 [
	"Test with multiple annotations"

	| res |
	res := self splitter start: 'abc**abc**xyz`xyz`last'.
	self
		assert: (res collect: [ :x | x printString ])
		equals: {'abc' . '[abc](bold)' . 'xyz' . '[xyz](monospace)' . 'last'}
]

{ #category : #tests }
MicInlineSplitterTest >> testSplitBlockAnchorReference [
	| res |
	res := self splitter start: 'abc*@anchorA@*def'.
	self
		assert: (res collect: [ :x | x printString ])
		equals: {'abc' . '[anchorA](anchorReference)' . 'def'}.
	self assert: (res second isKindOf: MicAnchorReferenceInlineBlock)

]

{ #category : #tests }
MicInlineSplitterTest >> testSplitBlockAnnotation [
	| res |
	res := self splitter start: 'abc<?an annotation?>def'.
	self
		assert: (res collect: [ :x | x printString ])
		equals: {'abc' . '[an annotation](annotation)' . 'def'}.
	self assert: (res second isKindOf: MicAnnotationInlineBlock)

]

{ #category : #tests }
MicInlineSplitterTest >> testSplitBlockBold [
	| res |
	res := self splitter start: 'abc**bold**def'.
	self
		assert: (res collect: [ :x | x printString ])
		equals: {'abc' . '[bold](bold)' . 'def'}.
	self assert: res second kind equals: #bold.

	res := self splitter start: 'abc** b old **def'.
	self
		assert: (res collect: [ :x | x printString ])
		equals: {'abc' . '[ b old ](bold)' . 'def'}.
	self assert: res second kind equals: #bold.

]

{ #category : #tests }
MicInlineSplitterTest >> testSplitBlockCode [
	| res |
	res := self splitter start: 'abc`block`def'.
	self
		assert: (res collect: [ :x | x printString ])
		equals: {'abc' . '[block](monospace)' . 'def'}.
]

{ #category : #tests }
MicInlineSplitterTest >> testSplitBlockImage [
	"Test the image annotation ![AltText](url)"

	| res |
	res := self splitter start: 'abc![AltText](my_directory/image.png)last'.
	self
		assert: (res collect: [ :x | x printString ])
		equals: {'abc' . '[AltText](figure)' . 'last'}.
	self assert: res second url class equals: ZnUrl.
	self assert: res second url segments first equals: 'my_directory'.
	self assert: res second url segments second equals: 'image.png'	
]

{ #category : #tests }
MicInlineSplitterTest >> testSplitBlockItalics [
	| res |
	res := self splitter start: 'abc_italics_def'.
	self
		assert: (res collect: [ :x | x printString ])
		equals: {'abc' . '[italics](italic)' . 'def'}.
	
	res := self splitter start: 'abc_ ita lics _def'.
	self
		assert: (res collect: [ :x | x printString ])
		equals: {'abc' . '[ ita lics ](italic)' . 'def'}.
]

{ #category : #tests }
MicInlineSplitterTest >> testSplitBlockLink [
	"Test the link annotation [LinkText](url)"

	| res |
	res := self splitter start: 'abc[LinkText](myURL)last'.
	self
		assert: (res collect: [ :x | x printString ])
		equals: {'abc' . '[LinkText](linkName)' . 'last'}.
	self assert: res second url class equals: ZnUrl.
	self assert: res second url segments first equals: 'myURL'
]

{ #category : #fails }
MicInlineSplitterTest >> testSplitBlockLinkIncomplete [
	"Test the annotation [LinkText] -> should return a link with url as linktext"

	| res |
	self skip.
	res := self splitter start: 'abc[LinkText]last'.
	self
		assert: (res collect: [ :x | x printString ])
		equals: {'abc' . '[LinkText](linkName)' . 'last'}.
	self assert: res second url equals: 'LinkText'
]

{ #category : #tests }
MicInlineSplitterTest >> testSplitBlockLinkInlineImage [
	"Test the link annotation [![alttext](imageurl)](url)"

	| res |
	res := self splitter
		start: 'abc[![alttext](imageurl)](url)last'.
	self
		assert: (res collect: [ :x | x printString ])
		equals: {'abc' . '[[alttext](figure)](linkName)' . 'last'}.
	self assert: res second children first url segments first equals: 'imageurl'
]

{ #category : #tests }
MicInlineSplitterTest >> testSplitBlockMultipleSequenceAnnotation [
	"Test with multiple annotations"

	| res |
	res := self splitter start: 'abc**abc**xyz`xyz`last'.
	self
		assert: (res collect: [ :x | x printString ])
		equals: {'abc' . '[abc](bold)' . 'xyz' . '[xyz](monospace)' . 'last'}
]

{ #category : #tests }
MicInlineSplitterTest >> testSplitBlockNested [
	"Test with nested annotations"
	| res |
	res := self splitter start: 'abc**bold_italic_bold**xyz'.
	self
		assert: (res collect: [ :x | x printString ])
		equals: {'abc' . '[bold [italic](italic) bold](bold)' . 'xyz'}
]

{ #category : #tests }
MicInlineSplitterTest >> testSplitBlockNested2 [
	"Test with nested annotations 2"
	| res |
	res := self splitter start: 'abc**x_y_z**cba'.
	self
		assert: (res collect: [ :x | x printString ])
		equals: {'abc' . '[x [y](italic) z](bold)' . 'cba'}
]

{ #category : #tests }
MicInlineSplitterTest >> testSplitBlockNoAnnotation [
	| res |
	res := self splitter start: 'abc'.
	self assert: res first printString equals: 'abc'.
	
	res := self splitter start: 'abc def gh'.
	self assert: res first printString equals: 'abc def gh'.
]

{ #category : #tests }
MicInlineSplitterTest >> testSplitBlockStrike [
	| res |
	res := self splitter start: 'abc~strike~def'.
	self
		assert: (res collect: [ :x | x printString ])
		equals: {'abc' . '[strike](strike)' . 'def'}.
]

{ #category : #tests }
MicInlineSplitterTest >> testSplitEmpty [
	| res |
	res := self splitter start: ''.
	self assert: res isEmpty
]

{ #category : #tests }
MicInlineSplitterTest >> testSplitExclamationMark [
	"Test that 'bla!bla' is ok, and not a failed image, same goes for '![goo]no parenthesis'"

	| res |
	res := self splitter start: 'bla!bla'.
	self
		assert: (res collect: [ :x | x printString ])
		equals: {'bla!bla'}.
	res := self splitter start: '![goo]no parenthesis'.
	self
		assert: (res collect: [ :x | x printString ])
		equals: {'![goo]no parenthesis'}
]

{ #category : #tests }
MicInlineSplitterTest >> testSplitMathCode [
	| res |
	res := self splitter start: 'abc$math env$def'.
	self
		assert: (res collect: [ :x | x printString ])
		equals: {'abc' . '[math env](math)' . 'def'}.
]

{ #category : #tests }
MicInlineSplitterTest >> testSplitMathRaw [
	| res |
	res := self splitter start: 'abc{{someRaw}}def'.
	self
		assert: (res collect: [ :x | x printString ])
		equals: {'abc' . '[someRaw](raw)' . 'def'}.
]

{ #category : #tests }
MicInlineSplitterTest >> testSplitNotClosed [
	| res |
	res := self splitter start: 'abc**xyz'.
	self assert: res first printString equals: 'abc**xyz'.
]

{ #category : #tests }
MicInlineSplitterTest >> testUrlObjectInUrlBlocks [

	| res |
	res := self splitter start: 'abc[test](myURL)last'.
	self assert: res second url class equals: ZnUrl.
	self assert: res second url segments first equals: 'myURL'
]
