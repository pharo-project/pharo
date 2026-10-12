"
Do not send super in the class side image start up (and shutdown) methods. This can break the state of the classes 
"
Class {
	#name : 'ReDoNotSendSuperShutDownInClassSideRule',
	#superclass : 'ReNodeRewriteRule',
	#instVars : [
		'ruleSelectors'
	],
	#category : 'General-Rules-Potential Bugs',
	#package : 'General-Rules',
	#tag : 'Potential Bugs'
}

{ #category : 'accessing' }
ReDoNotSendSuperShutDownInClassSideRule class >> groupSelector [

	^ #potentialBugsGroup
]

{ #category : 'accessing' }
ReDoNotSendSuperShutDownInClassSideRule class >> ruleName [

    ^ 'Class-side #shutDown: should not send "super shutDown: arg".'
]

{ #category : 'manifest' }
ReDoNotSendSuperShutDownInClassSideRule class >> uniqueIdentifierName [

    ^ 'ReDoNotSendSuperShutDownInClassSideRule'
]

{ #category : 'initialization' }
ReDoNotSendSuperShutDownInClassSideRule >> initialize [

    super initialize.
    self replace: 'super shutDown: `@arg.' with: ''.
    self replace: 'super shutDown' with: ''.
    self replace: 'super startUp: `@arg.' with: ''.
    self replace: 'super startUp' with: ''.

    ruleSelectors := { #shutDown:. #shutDown. #startUp:. #startUp }
]

{ #category : 'testing' }
ReDoNotSendSuperShutDownInClassSideRule >> shouldCheckMethod: aMethod [

    ^ (ruleSelectors includes: aMethod selector) and: [ aMethod methodClass isMeta ]
]
