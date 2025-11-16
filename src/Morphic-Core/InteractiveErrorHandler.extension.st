Extension { #name : 'InteractiveErrorHandler' }

{ #category : '*Morphic-Core' }
InteractiveErrorHandler >> onPrimitiveError: aString [

	| message |
	message := String streamContents: [ :s |
		           | context |
		           s nextPutAll: '*** System error handling failed ***'.
		           s
			           cr;
			           nextPutAll: aString.
		           context := thisContext sender sender.
		           20 timesRepeat: [
			           context ifNotNil: [
				           s
					           cr;
					           print: (context := context sender) ] ] ].
	Stdio stderr << message
]
