"
SUnit tests for RBComment
"
Class {
	#name : #RBCommentTest,
	#superclass : #TestCase,
	#category : #'AST-Core-Tests-Nodes'
}

{ #category : #tests }
RBCommentTest >> testIntersectsInterval [
	| node |
	node:= RBComment with: (RBCommentToken value: 'Some sample text' start: 5 stop: 21).

	self
		assert: (node intersectsInterval: (4 to: 6)) description: 'either side of interval';
		assert: (node intersectsInterval: (4 to: 5)) description: 'before and on interval';
		assert: (node intersectsInterval: (5 to: 6)) description: 'and and after interval';
		assert: (node intersectsInterval: (5 to: 5)) description: 'on interval interval';
		assert: (node intersectsInterval: (1 to: 4)) not description: 'not in interval'
]
