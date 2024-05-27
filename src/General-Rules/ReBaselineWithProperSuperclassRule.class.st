"
A custom baseline class should be a (direct or indirect) subclass of the predefined class BaselineOf. So use BaselineOf as the superclass for your baseline class.
"
Class {
	#name : 'ReBaselineWithProperSuperclassRule',
	#superclass : 'ReAbstractRule',
	#category : 'General-Rules-Migrated',
	#package : 'General-Rules',
	#tag : 'Migrated'
}

{ #category : 'testing - interest' }
ReBaselineWithProperSuperclassRule class >> checksClass [

	^ true
]

{ #category : 'manifest' }
ReBaselineWithProperSuperclassRule class >> uniqueIdentifierName [ 
	"This number should be unique and should change only when the rule completely change semantics"

	^'ReBaselineWithProperSuperclassRule'
]

{ #category : 'running' }
ReBaselineWithProperSuperclassRule >> basicCheck: aClass [

	aClass isClassSide ifTrue: [ ^ false ].
	aClass name = #BaselineOf ifTrue: [ ^ false ].
	
	^(aClass name beginsWith: 'BaselineOf') 
		and: [ (aClass inheritsFrom: BaselineOf) not ]
 
]

{ #category : 'accessing' }
ReBaselineWithProperSuperclassRule >> group [

	^ 'Design Flaws'
]

{ #category : 'accessing' }
ReBaselineWithProperSuperclassRule >> name [

	^ 'Custom Baseline class should be a subclass of predefined class BaselineOf'
 
]

{ #category : 'accessing' }
ReBaselineWithProperSuperclassRule >> severity [

	^ #information
]
