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
	#category : 'General-Rules-Migrated',
	#package : 'General-Rules',
	#tag : 'Migrated'
}

{ #category : 'running' }
ReStatementsAfterReturnConditionalRule >> basicCheck: aNode [

	aNode isMessage ifFalse: [ ^ false ].
	(self selectorList includes: aNode selector) ifFalse: [ ^ false ].
	aNode arguments do: [ :arg |
		(arg isBlock and: [ arg statements last isReturn ]) ifFalse: [ ^ false ] ].
	^ aNode ~= aNode methodNode statements last
]

{ #category : 'accessing' }
ReStatementsAfterReturnConditionalRule >> group [

	^ 'Potential Bugs'
]

{ #category : 'accessing' }
ReStatementsAfterReturnConditionalRule >> name [

	^ 'statements written after conditional return'
]

{ #category : 'utilities' }
ReStatementsAfterReturnConditionalRule >> selectorList [

	^ #(#ifTrue:ifFalse: #ifFalse:ifTrue: #ifNil:ifNotNil: #ifNotNil:ifNil: #ifEmpty:ifNotEmpty: #ifNotEmpty:ifEmpty: #ifExists:ifAbsent:)
]
