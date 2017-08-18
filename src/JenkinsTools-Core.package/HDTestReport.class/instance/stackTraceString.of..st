private
stackTraceString: err of: aTestCase
	^ String streamContents: [ :str | 
		| context |
		context := err signalerContext.
		[ context isNil or: [ context receiver == aTestCase and: [ context methodSelector == #runCase ] ] ] whileFalse: [
			[str print: context; lf.] ifError: [ str nextPutAll: 'PRINTING ERROR'; lf].
			context := context sender ] ] 