"
This is an abstract rule class to check for an appropriate method
protol name.

Subclasses should override #protocolIdiom to return an association of a good protocol name and the bad protocol names it should replace.
"
Class {
	#name : 'ReProperMethodProtocolNameRule',
	#superclass : 'ReAbstractRule',
	#instVars : [
		'methodProtocolName'
	],
	#category : 'Renraku-Rules',
	#package : 'Renraku',
	#tag : 'Rules'
}

{ #category : 'private - accessing' }
ReProperMethodProtocolNameRule class >> badMethodProtocolNames [

	^self protocolIdiom value
]

{ #category : 'testing - interest' }
ReProperMethodProtocolNameRule class >> checksMethod [
	"This one is abstract but subclasses should run as rules"

	^ self name ~= #ReProperMethodProtocolNameRule
]

{ #category : 'private - accessing' }
ReProperMethodProtocolNameRule class >> goodMethodProtocolName [

	^self protocolIdiom key
]

{ #category : 'accessing' }
ReProperMethodProtocolNameRule class >> group [

	^ 'Method Protocol Name'
]

{ #category : 'testing' }
ReProperMethodProtocolNameRule class >> isAbstract [

	^ self == ReProperMethodProtocolNameRule
]

{ #category : 'idioms' }
ReProperMethodProtocolNameRule class >> protocolIdiom [
	"Subclasses should override to return an association between a wished protocol and an array of protocol names that are not so good."

	^ self subclassResponsibility
]

{ #category : 'accessing' }
ReProperMethodProtocolNameRule class >> rationale [

	^ 'Check if the method protocol name is appropriate and fulfils common expectations.'
]

{ #category : 'accessing' }
ReProperMethodProtocolNameRule class >> ruleName [

	^ 'Method categorization: use ''' , self goodMethodProtocolName , ''' as protocol name instead of ''' , self badMethodProtocolNames asString , ''''
]

{ #category : 'private - utilities' }
ReProperMethodProtocolNameRule class >> use: valid insteadOf: arrayOfInvalid [
	"Instead of directly creating an array we use this utility method for better readability
	 in senders of this method"

	^valid -> arrayOfInvalid
]

{ #category : 'running' }
ReProperMethodProtocolNameRule >> basicCheck: aMethod [

	| protocol |
	protocol := aMethod protocolName.
	self methodProtocolName: protocol.
	^ self class badMethodProtocolNames includes: protocol
]

{ #category : 'helpers' }
ReProperMethodProtocolNameRule >> critiqueFor: aMethod [
	| proposedProtocol |
	proposedProtocol := self class goodMethodProtocolName asSymbol.

	^ (ReRefactoringCritique
		   withAnchor: (self anchorFor: aMethod)
		   by: self) refactoring: (RBMethodProtocolTransformation
			   protocol: { proposedProtocol }
			   inMethod: aMethod selector
			   inClass: aMethod methodClass name)
]

{ #category : 'accessing' }
ReProperMethodProtocolNameRule >> methodProtocolName [

	^ methodProtocolName
]

{ #category : 'accessing' }
ReProperMethodProtocolNameRule >> methodProtocolName: anObject [

	methodProtocolName := anObject
]

{ #category : 'accessing' }
ReProperMethodProtocolNameRule >> ruleName [

	^ 'Method categorization: use ''' , self class goodMethodProtocolName , ''' as protocol name instead of ''' , self methodProtocolName asString , ''''
]

{ #category : 'private - utilities' }
ReProperMethodProtocolNameRule >> use: valid insteadOf: arrayOfInvalid [
	"Instead of directly creating an array we use this utility method for better readability in senders of this method"

	^self class use: valid insteadOf: arrayOfInvalid
]
