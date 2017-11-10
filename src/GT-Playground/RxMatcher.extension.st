Extension { #name : #RxMatcher }

{ #category : #'*GT-Playground' }
RxMatcher >> matchesIn: aString subexpressionsDo: aBlock [
	self matchesOnStream: aString readStream subexpressionsDo: aBlock
]

{ #category : #'*GT-Playground' }
RxMatcher >> matchesOnStream: aStream subexpressionsDo: aBlock [
	| subexpressions |
	[ self searchStream: aStream ] whileTrue: [ 
		subexpressions := ((1 to: self subexpressionCount) collect: [ :each | (self subexpression: each) ]).
		aBlock value: subexpressions first value: subexpressions allButFirst ]
]
