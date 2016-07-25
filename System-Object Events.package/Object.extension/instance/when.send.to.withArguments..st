*System-Object Events
when: anEventSelector
send: aMessageSelector
to: anObject
withArguments: anArgArray
 
    self
        when: anEventSelector
        evaluate: (WeakMessageSend
            receiver: anObject
            selector: aMessageSelector
		arguments: anArgArray)