*System-Object Events
when: anEventSelector
send: aMessageSelector
to: anObject
 
    self
        when: anEventSelector
        evaluate: (WeakMessageSend
            receiver: anObject
            selector: aMessageSelector)