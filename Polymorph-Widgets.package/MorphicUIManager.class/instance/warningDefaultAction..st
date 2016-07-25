ui requests
warningDefaultAction: anException

"The user should be notified of the occurrence of an exceptional occurrence and given an option of continuing or aborting the computation. The description of the occurrence should include any text specified as the argument of the #signal: message."
	UIManager default
				debugProcess: Processor activeProcess
				context: anException signalerContext
				label: 'Warning'
				fullView: false 
				notification: anException messageText, '\\Select Proceed to continue, or close this window to cancel the operation.' withCRs.
				
	anException resume.