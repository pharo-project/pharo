"
A custom baseline class should be a (direct or indirect) subclass of the predefined class BaselineOf. So use BaselineOf as the superclass for your baseline class.
"
Class {
	#name : 'ReBaselineWithProperSuperclassRule',
	#superclass : 'ReAbstractRule',
	#category : 'General-Rules-Design Flaws',
	#package : 'General-Rules',
	#tag : 'Design Flaws'
}

{ #category : 'testing - interest' }
ReBaselineWithProperSuperclassRule class >> checksClass [

	^ true
]

{ #category : 'accessing' }
ReBaselineWithProperSuperclassRule class >> group [

	^ 'Design Flaws'
]

{ #category : 'accessing' }
ReBaselineWithProperSuperclassRule class >> ruleName [
	^ 'Custom Baseline class should be a subclass of predefined class BaselineOf'
]

{ #category : 'accessing' }
ReBaselineWithProperSuperclassRule class >> severity [

	^ #information
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
