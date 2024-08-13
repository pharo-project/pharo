"
Checks assignment to a variable that is the first statement inside the value block that is also used in the unwind block. For example

| inputStream  |
    [
    inputStream := self openStream. ""may fail !""
    inputStream doSomeOperation. ""may fail !""
    ] ensure: [ inputStream ifNotNil: [ inputStream close ]].

 if ""openStream"" fails, we don't need to ensure the stream is closed.
And otherwise, if we move the assignment outside of the block, we don't need
an ""ifNotNil""-check for the ensure block if ""doSomeOperation"" fails.
This code can be changed to

| inputStream  |
    inputStream := self openStream. ""may fail !""
    [inputStream doSomeOperation. ""may fail !""
    ] ensure: [ inputStream close ].
"
Class {
	#name : 'ReFileBlocksRule',
	#superclass : 'ReNodeMatchRule',
	#category : 'General-Rules-Potential Bugs',
	#package : 'General-Rules',
	#tag : 'Potential Bugs'
}

{ #category : 'accessing' }
ReFileBlocksRule class >> group [
	^ 'Potential Bugs'
]

{ #category : 'accessing' }
ReFileBlocksRule class >> rationale [
	^ 'Checks assignment to a variable that is the first statement inside the value block that is also used in the unwind block.'
]

{ #category : 'accessing' }
ReFileBlocksRule class >> ruleName [
	^ 'Assignment inside unwind blocks should be outside.'
]

{ #category : 'accessing' }
ReFileBlocksRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'FileBlocksRule'
]

{ #category : 'initialization' }
ReFileBlocksRule >> initialize [
	super initialize.
	self matchesAny: #(
			'[| `@temps |
				`var := `@object.
				`@.statements]
						ensure:
							[`var `@messages: `@args]'
			'[| `@temps |
				`var := `@object.
				`@.statements]
						ifCurtailed:
							[`var `@messages: `@args]' )
]
