"
IconStyler for highlight example expressions in method documents (seee PharoDocCommentNode). Enabled / Disabled by a Setting in group ""Code Browsing""
"
Class {
	#name : 'DocCommentIconStyler',
	#superclass : 'IconStyler',
	#category : 'PharoDocComment-Styling',
	#package : 'PharoDocComment',
	#tag : 'Styling'
}

{ #category : 'testing' }
DocCommentIconStyler class >> isStaticStyler [

	^ true
]

{ #category : 'private' }
DocCommentIconStyler >> exampleIsFaulty: aNode [
	^ aNode comments
		anySatisfy: [ :commentNode | commentNode pharoDocCommentNodes anySatisfy: [ :node |
				node expression isFaulty ]]
]

{ #category : 'defaults' }
DocCommentIconStyler >> highlightColor [
	^ (Color green alpha: 0.8)
]

{ #category : 'defaults' }
DocCommentIconStyler >> iconBlock: aNode [
	^ (self exampleIsFaulty: aNode)
		ifTrue: [ self notifySourceError: aNode ]
		ifFalse: [ self runExampleBlock: aNode ]
]

{ #category : 'defaults' }
DocCommentIconStyler >> iconFor: aNode [
	^ (self exampleIsFaulty: aNode)
		ifTrue: [ self iconNamed: #smallWarning ]
		ifFalse: [ self iconNamed: #smallInfo ]
]

{ #category : 'defaults' }
DocCommentIconStyler >> iconLabel: aNode [
	^ (self exampleIsFaulty: aNode)
		ifTrue: [ 'Faulty Example' ]
		ifFalse: [ 'Runnable Example' ]
]

{ #category : 'private' }
DocCommentIconStyler >> notifySourceError: aNode [
	"If a code in an example is faulty notify the user"

	^ [ | examples faultyExample |
		examples := aNode pharoDocCommentNodes.
		faultyExample := examples detect: [ :example | example expression isFaulty ].
		"FIXME: The parser do not show errors related to specific doccomments rules."
		OCParser parseExpression: faultyExample expression source ]
]

{ #category : 'private' }
DocCommentIconStyler >> runExampleBlock: aNode [
	^ [ | examples |
		examples := aNode pharoDocCommentNodes.
		(examples
			collect: [ :example |
			{ example expression expressionNode formattedCode . example expression evaluate}]) inspect]
]

{ #category : 'testing' }
DocCommentIconStyler >> shouldStyleNode: aNode [
	^ self stylingEnabled
		and: [ aNode isMethod
				and: [ aNode comments anySatisfy: [ :commentNode | commentNode hasDocComment ] ] ]
]

{ #category : 'private' }
DocCommentIconStyler >> stylingEnabled [
	^ PharoDocCommentNode docCommentEnabled
]
