"
IconStyler for highlight example expressions in method documents (seee PharoDocCommentNode). Enabled / Disabled by a Setting in group ""Code Browsing""
"
Class {
	#name : #DocCommentIconStyler,
	#superclass : #IconStyler,
	#category : #'PharoDocComment-Styling'
}

{ #category : #private }
DocCommentIconStyler >> exampleIsFaulty: aNode [
	<sampleInstance>
	^ aNode comments
		anySatisfy: [ :commentNode | commentNode pharoDocCommentNodes anySatisfy: [ :node |
				node expression expressionCode isFaulty ]]
]

{ #category : #defaults }
DocCommentIconStyler >> highlightColor [
	^ (Color green alpha: 0.8)
]

{ #category : #defaults }
DocCommentIconStyler >> iconBlock: aNode [
	^ (self exampleIsFaulty: aNode)
		ifTrue: [ self notifySourceError: aNode ]
		ifFalse: [ self runExampleBlock: aNode ]
]

{ #category : #defaults }
DocCommentIconStyler >> iconFor: aNode [
	^ (self exampleIsFaulty: aNode)
		ifTrue: [ self iconNamed: #smallWarning ]
		ifFalse: [ self iconNamed: #smallInfo ]
]

{ #category : #testing }
DocCommentIconStyler >> iconLabel: aNode [
	^ (self exampleIsFaulty: aNode)
		ifTrue: [ 'Faulty Example' ]
		ifFalse: [ 'Runnable Example' ]
]

{ #category : #private }
DocCommentIconStyler >> notifySourceError: aNode [
	"If a code in an example is faulty notify the user"
	"kjkl>>>2"
	"3 + 4 >>> 7"
	^ [ | examples faultyExample |
		examples := aNode pharoDocCommentNodes.
		faultyExample := examples detect: [ :example | example expression expressionCode isFaulty ].
		RBParser parseExpression: faultyExample expression expression ]
]

{ #category : #private }
DocCommentIconStyler >> runExampleBlock: aNode [
	^ [ | examples |
		examples := aNode pharoDocCommentNodes.
		(examples
			collect: [ :example | 
			{ example expression expressionCode formattedCode . example expression evaluate . example result expressionCode formattedCode}]) inspect]
]

{ #category : #testing }
DocCommentIconStyler >> shouldStyleNode: aNode [
	^ self stylingEnabled
		and: [ aNode isMethod
				and: [ aNode comments isNotEmpty
						and: [ aNode comments anySatisfy: [ :commentNode | commentNode pharoDocCommentNodes isNotEmpty ] ] ] ]
]

{ #category : #private }
DocCommentIconStyler >> stylingEnabled [
	^ PharoDocCommentNode docCommentEnabled 
]
