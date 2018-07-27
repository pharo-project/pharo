Class {
	#name : #STONCStyleCommentsSkipStreamTests,
	#superclass : #TestCase,
	#category : #'STON-Tests-Utilities'
}

{ #category : #testing }
STONCStyleCommentsSkipStreamTests >> testCommentInString [
	self 
		assert: (STONCStyleCommentsSkipStream on: '''/*comment*/''' readStream) upToEnd 
		equals: '''/*comment*/'''.
	self 
		assert: (STONCStyleCommentsSkipStream on: '{''foo'':''/*comment*/bar''}' readStream) upToEnd 
		equals: '{''foo'':''/*comment*/bar''}'.
	self 
		assert: (STONCStyleCommentsSkipStream on: '{''foo'':''//comment\''bar''}' readStream) upToEnd 
		equals: '{''foo'':''//comment\''bar''}'.
	self 
		assert: (STONCStyleCommentsSkipStream on: '{"foo":"/*comment*/bar"}' readStream) upToEnd 
		equals: '{"foo":"/*comment*/bar"}'.
	self 
		assert: (STONCStyleCommentsSkipStream on: '{"foo":"//comment\"bar"}' readStream) upToEnd 
		equals: '{"foo":"//comment\"bar"}'
]

{ #category : #testing }
STONCStyleCommentsSkipStreamTests >> testMultiLineComment [
	self 
		assert: (STONCStyleCommentsSkipStream on: 'foo\//comment\bar' withCRs readStream) upToEnd 
		equals: 'foo\bar' withCRs.
	self 
		assert: (STONCStyleCommentsSkipStream on: 'foo\//comment\bar' withCRs readStream) nextLine
		equals: 'foo'.
	self 
		assert: ((STONCStyleCommentsSkipStream on: 'foo\//comment\bar' withCRs readStream) nextLine; nextLine) 
		equals: 'bar'.
	self assert: (STONCStyleCommentsSkipStream on: '//comment' readStream) atEnd.
]

{ #category : #testing }
STONCStyleCommentsSkipStreamTests >> testNonComment [
	self assert: (STONCStyleCommentsSkipStream on: 'foo' readStream) upToEnd equals: 'foo'.
	self assert: (STONCStyleCommentsSkipStream on: 'foo' readStream) nextLine equals: 'foo'.
	self deny: (STONCStyleCommentsSkipStream on: 'foo' readStream) atEnd.
	self assert: (STONCStyleCommentsSkipStream on: '' readStream) atEnd.
]

{ #category : #testing }
STONCStyleCommentsSkipStreamTests >> testSTON [
	"Comments are processed first and totally removed.
	They are not even whitespace (not that whitespace is relevant in STON)."
	
	self 
		assert: (STON fromStream: (STONCStyleCommentsSkipStream on: '123/*comment*/456' readStream)) 
		equals: 123456.
	self 
		assert: (STON fromStream: (STONCStyleCommentsSkipStream on: '/*comment*/''abcd''' readStream)) 
		equals: 'abcd'.
	self 
		assert: (STON fromStream: (STONCStyleCommentsSkipStream on: '123//456' readStream)) 
		equals: 123.
	self 
		should: [ STON fromStream: (STONCStyleCommentsSkipStream on: '//456' readStream) ] 
		raise: STONReaderError

]

{ #category : #testing }
STONCStyleCommentsSkipStreamTests >> testSingleLineComment [
	self assert: (STONCStyleCommentsSkipStream on: 'foo/*comment*/bar' readStream) upToEnd equals: 'foobar'.
	self assert: (STONCStyleCommentsSkipStream on: 'foo/*comment*/bar' readStream) nextLine equals: 'foobar'.
	self deny: (STONCStyleCommentsSkipStream on: 'foo/*comment*/bar' readStream) atEnd.
	self assert: (STONCStyleCommentsSkipStream on: '/*comment*/' readStream) atEnd.
]
