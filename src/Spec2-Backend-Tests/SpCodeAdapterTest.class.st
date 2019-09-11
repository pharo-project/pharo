Class {
	#name : #SpCodeAdapterTest,
	#superclass : #SpTextAdapterTest,
	#category : #'Spec2-Backend-Tests'
}

{ #category : #tests }
SpCodeAdapterTest >> assertText: text at: index isStyle: styleName [
	| tag |

	tag := SpCodeStyleForTest new.
	text attributesAt: index do: [ :each | 
		self fillTag: tag withAttribute: each ].
	self assert: tag equals: (self style: styleName)
]

{ #category : #tests }
SpCodeAdapterTest >> assertText: text atInterval: interval isStyle: styleName [

	interval do: [ :index |
		self assertText: text at: index isStyle: styleName ]
]

{ #category : #accessing }
SpCodeAdapterTest >> classToTest [

	^ SpCodePresenter
]

{ #category : #tests }
SpCodeAdapterTest >> fillTag: tag withAttribute: attr [

	attr class = TextColor ifTrue: [ tag foreground: attr color ].
	self flag: #TODO. "Other attributes"
]

{ #category : #tests }
SpCodeAdapterTest >> style: aSymbol [ 
	| tag |
	
	tag := SpCodeStyleForTest new.
	self styleTableAt: aSymbol do: [ :each |
		self fillTag: tag withAttribute: each ]. 
	
	^ tag
	
]

{ #category : #tests }
SpCodeAdapterTest >> styleTableAt: styleName do: aBlock [ 
	| attributes |	

	attributes := (SHRBTextStyler initialTextAttributesForPixelHeight: 1) at: styleName.
	attributes do: aBlock
]

{ #category : #tests }
SpCodeAdapterTest >> testTextWithStyle [
	| text |

	presenter 
		behavior: nil;
		doItReceiver: Object;
		type: 'self asString.'.
	
	text := self adapter textWithStyle.
	self assertText: text atInterval: "self" (1 to: 4) isStyle: #self.
	self assertText: text atInterval: "asString" (6 to: 13) isStyle: #unary.

	presenter type: '10 + 42.0'.
	text := self adapter textWithStyle.
	self assertText: text atInterval: "10" (1 to: 2) isStyle: #integer.
	self assertText: text atInterval: "+" (4 to: 4) isStyle: #binary.
	self assertText: text atInterval: "42.0" (6 to: 9) isStyle: #number.

	presenter behavior: Object.
	presenter type: 'm1 ^ "test" 42'.
	text := self adapter textWithStyle.
	self assertText: text atInterval: "m1" (1 to: 2) isStyle: #patternKeyword.
	self assertText: text atInterval: "^" (4 to: 4) isStyle: #return.
	self assertText: text atInterval: "test" (6 to: 11) isStyle: #comment.	
	self assertText: text atInterval: "42" (13 to: 14) isStyle: #number.
]

{ #category : #tests }
SpCodeAdapterTest >> testWithSyntaxHighlight [
	presenter withSyntaxHighlight.
	self assert: self adapter hasSyntaxHighlightEnabled
]

{ #category : #tests }
SpCodeAdapterTest >> testWithoutSyntaxHighlight [
	presenter withoutSyntaxHighlight.
	self deny: self adapter hasSyntaxHighlightEnabled
]
