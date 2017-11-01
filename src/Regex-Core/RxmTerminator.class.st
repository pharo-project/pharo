"
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
--
Instances of this class are used to terminate matcher's chains. When a match reaches this (an instance receives #matchAgainst: message), the match is considered to succeed. Instances also support building protocol of RxmLinks, with some restrictions.
"
Class {
	#name : #RxmTerminator,
	#superclass : #Object,
	#category : 'Regex-Core'
}

{ #category : #matching }
RxmTerminator >> matchAgainst: aStream [
	"If got here, the match is successful."

	^true
]

{ #category : #building }
RxmTerminator >> pointTailTo: anRxmLink [
	"Branch tails are never redirected by the build algorithm.
	Healthy terminators should never receive this."

	RxParser signalCompilationException:
		'internal matcher build error - redirecting terminator tail'
]

{ #category : #building }
RxmTerminator >> terminateWith: aTerminator [
	"Branch terminators are never supposed to change.
	Make sure this is the case."

	aTerminator ~~ self
		ifTrue: [RxParser signalCompilationException:
				'internal matcher build error - wrong terminator']
]
