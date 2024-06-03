"
Use cascaded nextPutAll:''s instead of #, in #nextPutAll:.
Indeed this is really important to understand that the implementation of the concatenation of strings creates a new string for each uses of ,.
Therefore if you use five , to concatenate an expression you create 5 intermediate strings for nothing. Using nextPutAll: just add the string in the stream.

Have a look at  streamContents:. Here is an example

String streamContents: [ :s|
		s nextPutAll: '---'.
		s nextPutAll: self.
		s nextPutAll: '---' ].
"
Class {
	#name : 'ReCascadedNextPutAllsRule',
	#superclass : 'ReNodeRewriteRule',
	#category : 'General-Rules-Coding Idiom Violation',
	#package : 'General-Rules',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'accessing' }
ReCascadedNextPutAllsRule class >> group [
	^ 'Coding Idiom Violation'
]

{ #category : 'accessing' }
ReCascadedNextPutAllsRule class >> rationale [
	^ 'Use cascaded nextPutAll:''s instead of #, in #nextPutAll:.'
]

{ #category : 'accessing' }
ReCascadedNextPutAllsRule class >> ruleName [
	^ 'Use cascaded nextPutAll:''s instead of #, in #nextPutAll:'
]

{ #category : 'accessing' }
ReCascadedNextPutAllsRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'CascadedNextPutAllsRule'
]

{ #category : 'initialization' }
ReCascadedNextPutAllsRule >> initialize [
	super initialize.
	self
		replace: '``@rcvr nextPutAll: ``@object1 , ``@object2' with: '``@rcvr nextPutAll: ``@object1; nextPutAll: ``@object2';
		replace: '``@rcvr show: ``@object1 , ``@object2' with: '``@rcvr show: ``@object1; show: ``@object2'
]
