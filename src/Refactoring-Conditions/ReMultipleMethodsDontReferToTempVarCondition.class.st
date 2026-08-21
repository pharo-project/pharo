"
I check if temporary variable name is used in other methods of the class.
"
Class {
	#name : 'ReMultipleMethodsDontReferToTempVarCondition',
	#superclass : 'ReVariableNameCondition',
	#instVars : [
		'class',
		'selector'
	],
	#category : 'Refactoring-Conditions',
	#package : 'Refactoring-Conditions'
}

{ #category : 'instance creation' }
ReMultipleMethodsDontReferToTempVarCondition class >> name: aString class: aClass selector: aSelector [

	^ (self name: aString)
		  class: aClass;
		  selector: aSelector yourself
]

{ #category : 'checking' }
ReMultipleMethodsDontReferToTempVarCondition >> check [

	^ self violators isEmpty
]

{ #category : 'setter' }
ReMultipleMethodsDontReferToTempVarCondition >> class: aRBClass [ 

	class := aRBClass.

]

{ #category : 'utilities' }
ReMultipleMethodsDontReferToTempVarCondition >> multipleMethodsDefiningTemporary: aString in: aClass ignore: aBlock [

	| searcher methods method |
	searcher := OCParseTreeSearcher new.
	methods := Set new.
	method := nil.

	searcher matches: aString do: [ :aNode :answer | methods add: method ].
	aClass withAllSubclasses do: [ :cls |
			cls selectors do: [ :each |
					(aBlock value: cls value: each) ifFalse: [
							| parseTree |
							method := cls methodFor: each.
							parseTree := cls parseTreeForSelector: each.
							parseTree ifNotNil: [ searcher executeTree: parseTree ] ] ] ].
	^ methods
]

{ #category : 'accessing' }
ReMultipleMethodsDontReferToTempVarCondition >> selector: aSelector [

	selector := aSelector
]

{ #category : 'displaying' }
ReMultipleMethodsDontReferToTempVarCondition >> violationMessageOn: aStream [

	self violators ifEmpty: [ ^ self ].
	^ aStream
		  nextPutAll: 'More than one method defines temporary variable ';
		  nextPutAll: name;
		  nextPutAll: ': ';
		  nextPutAll: (self violators collect: [ :m | m selector ]) asArray asString
]

{ #category : 'checking' }
ReMultipleMethodsDontReferToTempVarCondition >> violators [

	| methods |
	methods := self multipleMethodsDefiningTemporary: name in: class ignore: [ :cls :selectors | false ].
	^ methods reject: [ :m | m selector = selector ]
]
