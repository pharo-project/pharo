*System-Object Events
when: anEventSelector
send: aMessageSelector
to: anObject
with: anArg
 
    self
        when: anEventSelector
        evaluate: (WeakMessageSend
            receiver: anObject
            selector: aMessageSelector
		arguments: {anArg})