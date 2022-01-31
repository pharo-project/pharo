Class {
	#name : #MicEnvironmentBlockTest,
	#superclass : #MicBlockTest,
	#category : #'Microdown-Tests-Parser'
}

{ #category : #'tests - default environment' }
MicEnvironmentBlockTest >> testEnvironment [
	"'<?card
blbababbab bababba
?>
'" 
	| source root env environmentName |
	environmentName := 'card'.
	source := parser environmentOpeningBlockMarkup , environmentName, String cr, 
	 'blbababbab bababba', String cr,
	parser environmentClosingBlockMarkup, String cr.
	root :=  parser parse: source.
	self assert: root children size equals: 1.
	env := root children first. 
	self assert: (env isKindOf: MicEnvironmentBlock). 
	self assert: env name equals: environmentName.
	self assert: env children first children first text equals: 'blbababbab bababba'
	
]

{ #category : #'tests - default environment' }
MicEnvironmentBlockTest >> testEnvironmentClosingNotEatingClosingCharacter [

	
	| root env |
	root := parser parse: '<?card
to

?>'.
	self assert: root children size equals: 1.
	env := root children first.
	self assert: env children first text equals: 'to'.
]

{ #category : #'tests - extensions' }
MicEnvironmentBlockTest >> testEnvironmentIsCreatedWhenTagDoesNotMatch [

	| source root env environmentName |
	environmentName := 'slideXXX'.
	source := parser environmentOpeningBlockMarkup , environmentName, String cr, 
	 'this is the slide body', String cr,
	parser environmentClosingBlockMarkup, String cr.
	
	root :=  parser parse: source.
	self assert: root children size equals: 1.
	env := root children first. 
	self assert: (env isKindOf: MicEnvironmentBlock). 
	self assert: env name equals: environmentName.
	self assert: env children first children first text equals: 'this is the slide body'
	
]

{ #category : #'tests - extensions' }
MicEnvironmentBlockTest >> testEnvironmentWiathArgsWithCorrespondingClassIsCreated [

		| source root environment environmentName contents |
	environmentName := 'slide'.
	contents := 'blbababbab
bababba

toto'.
	source := parser environmentOpeningBlockMarkup, environmentName, parser argumentListStartDelimiter, 'a=1&b=17&c'
		, String cr , contents , String cr
		, parser environmentClosingBlockMarkup , String cr.
		
	root := parser parse: source.
	self assert: root children size equals: 1.
	environment := root children first.
	self assert: environment name equals: 'slide'.
	self assert: environment class equals: MicSlideBlock.
	self assert: (environment arguments at: 'a') equals: '1'.
	self assert: (environment arguments at: 'b') equals: '17'.
	self assert: (environment arguments at: 'c') equals: nil.
	
]

{ #category : #'tests - default environment' }
MicEnvironmentBlockTest >> testEnvironmentWithAnyNestedElement [
	"'<?slide
- item 1 
- item 2
?>
'" 
	| source root env environmentName |
	environmentName := 'slide'.
	source := parser environmentOpeningBlockMarkup , environmentName, String cr, 
	 '- item1 
- item 2

blbababbab bababba', String cr,
	parser environmentClosingBlockMarkup, String cr.
	root :=  parser parse: source.
	
	self assert: root children size equals: 1.
	env := root children first. 
	self assert: (env isKindOf: MicEnvironmentBlock). 
	self assert: env name equals: environmentName.
	
	self assert: env children size equals: 2.
	self assert: env children first class equals: MicUnorderedListBlock.
	self assert: env children first children size equals: 2.
	self assert: env children first children first class equals: MicListItemBlock.
	self assert: env children second class equals: MicParagraphBlock.
	self assert: env children second children first class equals: MicTextBlock.
	
]

{ #category : #'tests - default environment' }
MicEnvironmentBlockTest >> testEnvironmentWithArguments [
	 "'<?card|a=1&b=17&c
blbababbab
bababba

toto
?>
'"
	
	| source root environment environmentName contents |
	environmentName := 'card'.
	contents := 'blbababbab
bababba

toto'.
	source := parser environmentOpeningBlockMarkup, environmentName, parser argumentListStartDelimiter, 'a=1&b=17&c'
		, String cr , contents , String cr
		, parser environmentClosingBlockMarkup , String cr.
	root := parser parse: source.
	self assert: root children size equals: 1.
	environment := root children first.
	self assert: environment name equals: 'card'.
	self assert: (environment arguments at: 'a') equals: '1'.
	self assert: (environment arguments at: 'b') equals: '17'.
	self assert: (environment arguments at: 'c') equals: nil.
]

{ #category : #'tests - default environment' }
MicEnvironmentBlockTest >> testEnvironmentWithCodeBlock [
 
	| root env |
	root :=  parser parse: '<?slide

This is a nice piece of code
```
1000 factorial /999 factorial
>>> 1000
```
isn''t it
?>
'.
	
	self assert: root children size equals: 1.
	env := root children first. 	
	self assert: env children size equals: 3.
	self assert: env children first class equals: MicParagraphBlock.
	self assert: env children second class equals: MicCodeBlock.
	self assert: env children third class equals: MicParagraphBlock.
	
]

{ #category : #'tests - extensions' }
MicEnvironmentBlockTest >> testEnvironmentWithCorrespondingClassIsCreated [

	| source root env environmentName |
	environmentName := 'slide'.
	source := parser environmentOpeningBlockMarkup ,  environmentName, String cr, 
	 'this is the slide body', String cr,
	parser environmentClosingBlockMarkup, String cr.
	
	root :=  parser parse: source.
	self assert: root children size equals: 1.
	env := root children first. 
	self assert: (env isKindOf: MicSlideBlock). 
	self assert: env name equals: environmentName.
	self assert: env children first children first text equals: 'this is the slide body'
	
]

{ #category : #'tests - default environment' }
MicEnvironmentBlockTest >> testEnvironmentWithFormat [
	"'<?slide
- item 1 
- item 2
?>
'" 
	| source root env environmentName |
	environmentName := 'slide'.
	source := parser environmentOpeningBlockMarkup , environmentName, String cr, 
	 '- item1 **bold**
- item 2

blbababbab bababba', String cr,
	parser environmentClosingBlockMarkup, String cr.
	root :=  parser parse: source.
	
	self assert: root children size equals: 1.
	env := root children first. 

	self assert: env children first children first class equals: MicListItemBlock.
	self assert: env children first children first textElements second class equals: MicBoldFormatBlock 
	
]

{ #category : #'tests - default environment' }
MicEnvironmentBlockTest >> testEnvironmentWithMultipleLines [
	"'<?card
blbababbab
bababba

toto
?>
'"
	
	| source root env environmentName contents |
	environmentName := 'card'.
	contents := 'blbababbab
bababba

toto'.
	source := parser environmentOpeningBlockMarkup , environmentName
		, String cr , contents , String cr
		, parser environmentClosingBlockMarkup , String cr.
	root := parser parse: source .
	self assert: root children size equals: 1.
	env := root children first.
	self assert: (env isKindOf: MicEnvironmentBlock).
	self assert: env name equals: environmentName.
	self assert: env children size equals: 2.
	self assert: env children first class equals: MicParagraphBlock.
	self assert: env children second class equals: MicParagraphBlock.
	self assert: env children second text equals: 'toto'.
]

{ #category : #'tests - default environment' }
MicEnvironmentBlockTest >> testEnvironmentWithNoEmptyBeforeTheEndMarker [

	
	| root env |
	root := parser parse: '<?card
Here we are a paragraph and the paragraph should not eat the end of the
environment, else we force writer to enter a new line to declare the end of
the paragraph.
?>'.
	self assert: root children size equals: 1.
	env := root children first.
	self assert: env children first text equals: 'Here we are a paragraph and the paragraph should not eat the end of the
environment, else we force writer to enter a new line to declare the end of
the paragraph.'.
]

{ #category : #'tests - extensions' }
MicEnvironmentBlockTest >> testExtensionClassWithSpaceAndArgIsCreated [

		| source root environment environmentName contents |
	environmentName := ' slide'.
	contents := 'blbababbab
bababba

toto'.
	source := parser environmentOpeningBlockMarkup, environmentName, parser argumentListStartDelimiter, 'a=1&b=17&c'
		, String cr , contents , String cr
		, parser environmentClosingBlockMarkup , String cr.
		
	root := parser parse: source.
	self assert: root children size equals: 1.
	environment := root children first.
	self assert: environment name equals: 'slide'.
	self assert: environment class equals: MicSlideBlock.
	self assert: (environment arguments at: 'a') equals: '1'.
	self assert: (environment arguments at: 'b') equals: '17'.
	self assert: (environment arguments at: 'c') equals: nil.
	
]

{ #category : #'tests - default environment' }
MicEnvironmentBlockTest >> testParagraphEnvironmentParagraph [
	"
'"
	
	| root |

	root := parser parse: 'What a super day for a ride.
	
<?card
Sunn Special S3

?>

Yes this is a good idea.'.
	
	self assert: root children size equals: 3.
	self assert: root children second children first text equals: 'Sunn Special S3'.
	self assert: root children third text  equals: 'Yes this is a good idea.'.
	

]

{ #category : #'tests - default environment' }
MicEnvironmentBlockTest >> testedClass [ 
	^ MicEnvironmentBlock  
]
