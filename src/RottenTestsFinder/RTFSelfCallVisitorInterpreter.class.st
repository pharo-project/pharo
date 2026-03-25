"
I provide a template to ve overridden for visiting messages nodes sent to self
"
Class {
	#name : 'RTFSelfCallVisitorInterpreter',
	#superclass : 'OCProgramNodeVisitor',
	#instVars : [
		'selfClass',
		'selfSentMethods',
		'considerClassesThat',
		'selfSentMethod'
	],
	#category : 'RottenTestsFinder-SelfInterpreter',
	#package : 'RottenTestsFinder',
	#tag : 'SelfInterpreter'
}

{ #category : 'private' }
RTFSelfCallVisitorInterpreter >> analyze [

	self selfSentMethods add: selfSentMethod.
]

{ #category : 'accessing' }
RTFSelfCallVisitorInterpreter >> considerClassesThat [

	^ considerClassesThat
]

{ #category : 'accessing' }
RTFSelfCallVisitorInterpreter >> considerClassesThat: anObject [
	considerClassesThat := anObject
]

{ #category : 'initialization' }
RTFSelfCallVisitorInterpreter >> initialize [ 

	super initialize.
	considerClassesThat := [ :class | class inheritsFrom: Object ].
	selfSentMethods := OrderedCollection new
]

{ #category : 'private' }
RTFSelfCallVisitorInterpreter >> isMethodAnalyzable [

	(self considerClassesThat value: selfSentMethod methodClass)
		ifFalse: [ ^ false ].
	selfSentMethod isPrimitive
		ifTrue: [ ^ false ].
	(self selfSentMethods includes: selfSentMethod)
		ifTrue: [ ^ false ].
	^ true
]

{ #category : 'private' }
RTFSelfCallVisitorInterpreter >> lookupMethodFromNode: aMessageNode [

	| lookupClass |
	lookupClass := aMessageNode receiver isSuperVariable
		               ifTrue: [ self selfClass superclass ]
		               ifFalse: [ self selfClass ].

	selfSentMethod := lookupClass lookupSelector: aMessageNode selector
]

{ #category : 'private' }
RTFSelfCallVisitorInterpreter >> preAnalysisHook: aMessageNode [

	^ self
]

{ #category : 'api' }
RTFSelfCallVisitorInterpreter >> reset [

	self selfSentMethods removeAll
]

{ #category : 'accessing' }
RTFSelfCallVisitorInterpreter >> selfClass [

	^ selfClass ifNil: [ Error signal: 'self class not assigned' ]
]

{ #category : 'accessing' }
RTFSelfCallVisitorInterpreter >> selfClass: aClass [

	selfClass := aClass 
]

{ #category : 'accessing' }
RTFSelfCallVisitorInterpreter >> selfSentMethods [

	^ selfSentMethods 
]

{ #category : 'visiting' }
RTFSelfCallVisitorInterpreter >> visitMessageNode: aMessageNode [

	^ self visitMessageNode: aMessageNode ifMissingMethod: [
			Warning signal: ('#{1} can not be found in {2} nor in any super class.' format: {
						 aMessageNode selector.
						 selfClass name }).
			^ self ]
]

{ #category : 'visiting' }
RTFSelfCallVisitorInterpreter >> visitMessageNode: aMessageNode ifMissingMethod: aBlock [

	self visitNodes: aMessageNode.
	(aMessageNode receiver isSelfVariable 
		or: [ aMessageNode receiver isSuperVariable ]) 
			ifFalse: [ ^ self ].
	self lookupMethodFromNode: aMessageNode.
	selfSentMethod ifNil: [
			aBlock value.
			^ self ].
	self preAnalysisHook: aMessageNode.
	self isMethodAnalyzable 
		ifTrue: [ self analyze ]
]

{ #category : 'visiting' }
RTFSelfCallVisitorInterpreter >> visitNodes: aMessageNode [

	aMessageNode receiver acceptVisitor: self.
	aMessageNode arguments do: [ :each | each acceptVisitor: self ]
]
