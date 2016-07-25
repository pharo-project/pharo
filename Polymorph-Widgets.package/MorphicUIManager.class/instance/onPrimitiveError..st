events
onPrimitiveError: aString 
	| message |
	message := String
				streamContents: [:s | 
					| context |
					s nextPutAll: '*** System error handling failed ***'.
					s cr; nextPutAll: aString.
					context := thisContext sender sender.
					20
						timesRepeat: [context == nil
								ifFalse: [s cr; print: (context := context sender)]]].
	Transcripter askForEmergencyEvaluatorOrExitWithText: message.
	"init hands and redisplay" 
	World install
