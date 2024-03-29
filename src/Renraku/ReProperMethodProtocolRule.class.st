"
Abstract superclass for rules checking if a specific instance or class method is properly categorized into a specific protocol
"
Class {
	#name : 'ReProperMethodProtocolRule',
	#superclass : 'ReAbstractRule',
	#category : 'Renraku-Rules',
	#package : 'Renraku',
	#tag : 'Rules'
}

{ #category : 'testing' }
ReProperMethodProtocolRule class >> checksClassMethod [

	^ self subclassResponsibility
]

{ #category : 'testing' }
ReProperMethodProtocolRule class >> checksInstanceMethod [

	^ self checksClassMethod not
]

{ #category : 'testing - interest' }
ReProperMethodProtocolRule class >> checksMethod [

	^ true
]

{ #category : 'testing' }
ReProperMethodProtocolRule class >> isAbstract [

	^ self == ReProperMethodProtocolRule
]

{ #category : 'running' }
ReProperMethodProtocolRule >> basicCheck: aMethod [

	| rightSide |
	self class isAbstract ifTrue: [^ false ].
	rightSide := self class checksClassMethod
		        ifTrue: [ aMethod isClassSide ]
		        ifFalse: [ aMethod isClassSide not ].

	^ rightSide and: [
		  aMethod selector = self selector and: [
			  aMethod protocolName ~= self protocolName asSymbol ] ]
]

{ #category : 'helpers' }
ReProperMethodProtocolRule >> critiqueFor: aMethod [

	| methodSymbol classSymbol |
	methodSymbol := aMethod selector asSymbol.
	classSymbol := aMethod methodClass name asSymbol.

	^ (ReRefactoringCritique
		   withAnchor: (self anchorFor: aMethod)
		   by: self) refactoring: (RBMethodProtocolTransformation
			   protocol: self protocolName
			   inMethod: methodSymbol
			   inClass: classSymbol)
]

{ #category : 'accessing' }
ReProperMethodProtocolRule >> group [

	^ 'Clean Code'
]

{ #category : 'accessing' }
ReProperMethodProtocolRule >> name [

	| side |
	side := self class checksClassMethod
		        ifTrue: [ 'Class' ]
		        ifFalse: [ 'Instance' ].

	^ side , ' side methods called #' , self selector asString
	  , ' should be in the ''' , self protocolName , ''' protocol'
]

{ #category : 'accessing - defaults' }
ReProperMethodProtocolRule >> protocolName [

	^ self subclassResponsibility
]

{ #category : 'accessing - defaults' }
ReProperMethodProtocolRule >> selector [

	^ self subclassResponsibility
]
