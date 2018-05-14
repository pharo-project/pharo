"
I represent a example expression (expression and value) within a method
comment (RBCommentNode). Every pair of Pharo expression and a separator (default "">>>"") can be styled as Pharo code and the editor iconstyler  can highlight the code and evaluate the expression.

Example, within a method comment:

"" 3 + 4 >>> 7""

The example code should show an  expression 3 + 4 and the expected value 7.

See setting on ""Code Browsing"" to enable / disable this doc comment style.
"
Class {
	#name : #PharoDocCommentNode,
	#superclass : #Object,
	#instVars : [
		'sourceNode',
		'expression',
		'result'
	],
	#classVars : [
		'DocCommentEnabled'
	],
	#category : #'PharoDocComment-Base'
}

{ #category : #settings }
PharoDocCommentNode class >> docCommentEnabled [
	^ DocCommentEnabled  ifNil: [ DocCommentEnabled  := true ] 
]

{ #category : #settings }
PharoDocCommentNode class >> docCommentEnabled: aBoolean [
	DocCommentEnabled := aBoolean
]

{ #category : #parsing }
PharoDocCommentNode class >> docCommentRangesIn: aText [
	^ '(([^[:cntrl:]]*)>>>\3?([^[:cntrl:]]*))' asRegex matchingRangesIn: aText
]

{ #category : #'instance creation' }
PharoDocCommentNode class >> expression: aDocExpression result: anotherDocExpression inComment: aRBComment [
	^ self new
		expression: aDocExpression;
		result: anotherDocExpression;
		sourceNode: aRBComment
		yourself
]

{ #category : #parsing }
PharoDocCommentNode class >> parseDocComments: aRBCommentNode [
	| ranges |
	ranges := self docCommentRangesIn: aRBCommentNode contents.
	^ ranges
		collect: [ :range | 
			| expression result exps |
			exps := '>>>' split: (aRBCommentNode contents copyFrom: range first to: range last).
			expression := PharoDocCommentExpression new
				expressionInterval: (range first to: range first + exps first size - 1);
				source: aRBCommentNode contents.
			result := PharoDocCommentExpression new
				expressionInterval: (1 + range last - exps last size to: range last);
				source: aRBCommentNode contents.
			self expression: expression result: result inComment: aRBCommentNode ]
]

{ #category : #settings }
PharoDocCommentNode class >> settingsOn: aBuilder [
	<systemsettings>
	(aBuilder setting: #docCommentEnabled)
		target: self;
		description:
			'Allow styled and executable example expressions within method comments. 
		Example: 
		"3+4 >>> 7."';
		label: 'Pharo doc comments';
		parent: #codeBrowsing
]

{ #category : #conversion }
PharoDocCommentNode >> asTriplet [
	"Return the expression, its returned value and its expect result."
	^ { self expression expressionCode formattedCode . self expression evaluate . self result expressionCode formattedCode}
]

{ #category : #accessing }
PharoDocCommentNode >> expression [
	^ expression
]

{ #category : #accessing }
PharoDocCommentNode >> expression: anObject [
	expression := anObject
]

{ #category : #printing }
PharoDocCommentNode >> printOn: aStream [

	super printOn: aStream.
	aStream nextPutAll: '('.
	aStream nextPutAll: self expression source.
	aStream nextPutAll: ')'.
]

{ #category : #accessing }
PharoDocCommentNode >> result [
	^ result
]

{ #category : #accessing }
PharoDocCommentNode >> result: anObject [
	result := anObject
]

{ #category : #accessing }
PharoDocCommentNode >> sourceNode [
	^ sourceNode
]

{ #category : #accessing }
PharoDocCommentNode >> sourceNode: anObject [
	sourceNode := anObject
]
