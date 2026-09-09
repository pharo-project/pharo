"
This logger forwards to the Transcript all signals that it receives.

### Example:

```
TranscriptLogger new 
    runDuring: [ StringSignal emit: 'This is a message' ]
```
"
Class {
	#name : 'TranscriptLogger',
	#superclass : 'SignalLogger',
	#category : 'Beacon-Core-Loggers',
	#package : 'Beacon-Core',
	#tag : 'Loggers'
}

{ #category : 'logging' }
TranscriptLogger >> nextPut: anAnnouncement [

	anAnnouncement crTrace
]
