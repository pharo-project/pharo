"
A CodeSearchingRule is rule using a RBParseTreeSearcher
for the search.

use #matcher: for setting the parse tree searcher.

"
Class {
	#name : #CodeSearchingRule,
	#superclass : #RBParseTreeLintRule,
	#category : 'NautilusRefactoring'
}

{ #category : #testing }
CodeSearchingRule class >> isVisible [
	^ false
]

{ #category : #accessing }
CodeSearchingRule class >> uniqueIdentifierName [
	
	^ 'CodeSearchingRule'
]

{ #category : #accessing }
CodeSearchingRule >> matcher: aRule [

	matcher := aRule
]

{ #category : #accessing }
CodeSearchingRule >> name [

	^ 'Code Searching'
]
