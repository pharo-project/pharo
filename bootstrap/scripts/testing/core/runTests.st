'From Pharo 6.0 of 13 May 2016 [Latest update: #60182] on 16 August 2016 at 3:15:10.542222 pm'!

!HDTestReport methodsFor: 'private' stamp: 'CamilloBruni 11/4/2013 08:27'!
stackTraceString: err of: aTestCase
	^ String streamContents: [ :str | 
		| context |
		context := err signalerContext.
		[ context isNil or: [ context receiver == aTestCase and: [ context methodSelector == #runCase ] ] ] whileFalse: [
			[str print: context; lf.] ifError: [ str nextPutAll: 'PRINTING ERROR'; lf].
			context := context sender ] ]  ! !


'From Pharo6.0 of 13 May 2016 [Latest update: #60069] on 13 July 2016 at 9:43:54.170076 am'!

!HDTestReport methodsFor: 'running' stamp: 'AndreiChis 10/7/2013 11:59'!
serializeError: error of: aTestCase


! !

ClassTestCase removeSelector: #testClassComment.
HashTesterTest removeSelector: #testBasicBehaviour.


(RPackage organizer packages collect: #name) logCr.

HDTestReport runPackages: (RPackage organizer packages collect: #name).

"HDTestReport runClasses: Smalltalk allClasses named: 'Tests'."

Smalltalk exitSuccess. 