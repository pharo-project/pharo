"
This smell arise when statements are written after a condition that return within 2 branch.

For exemple : 

method 
	1 = 1 
	fTrue: [ ^ 1 ] ifFalse: [ ^ 2 ]. 
	2
"
Class {
	#name : 'ReStatementsAfterReturnConditionalRule',
	#superclass : 'ReNodeBasedRule',
	#category : 'General-Rules-Potential Bugs',
	#package : 'General-Rules',
	#tag : 'Potential Bugs'
}

{ #category : 'accessing' }
ReStatementsAfterReturnConditionalRule class >> group [

	^ 'Potential Bugs'
]

{ #category : 'accessing' }
ReStatementsAfterReturnConditionalRule class >> ruleName [
	^ 'statements written after conditional return'
]

{ #category : 'running' }
ReStatementsAfterReturnConditionalRule >> basicCheck: aNode [

	aNode isMessage ifFalse: [ ^ false ].
	(self selectorList includes: aNode selector) ifFalse: [ ^ false ].
	(aNode arguments anySatisfy: [ :arg |
		 (arg isBlock and: [
			  arg statements isNotEmpty and: [ arg statements last isReturn ] ]) not ]) ifTrue: [ ^ false ].
	^ aNode ~= aNode methodNode statements last
]

{ #category : 'utilities' }
ReStatementsAfterReturnConditionalRule >> selectorList [

	^ #(#ifTrue:ifFalse: #ifFalse:ifTrue: #ifNil:ifNotNil: #ifNotNil:ifNil: #ifEmpty:ifNotEmpty: #ifNotEmpty:ifEmpty: #ifExists:ifAbsent:)
]
