I'm an output device class which can send textural inputs to different outputs.
By default, there is one global instance of ThreadSafeTranscript which can be reached by using the global variable Transcript, and used to send text to the outputs it has, by for example: Transcript show: 'hello'.

I implement most of the common stream api, and is threadsafe wrt stream access.

A transcript can have multiple outputs which receive anything sent to the transcript. Anything implementing TTranscriptOutput can be an output for the transcript.  Every output connected to a transcript will be updated with the current content when the transcript entry is ended, cleared, or flushed.

There is also support for a update strategy which can be set for a certain time by using #updateUsing: while:, one of its use cases is when one wants to update outputs which are present in the world, but the code that is sending something is also blocking the UI thread.
 for example: 
	| tr |
	tr := TranscriptStream new.
	tr open.
	10 timesRepeat: [ tr crShow:  'HelloWorld' . 1 second asDelay wait ].

instead: 	
	tr := TranscriptStream new.
	tr open.
	tr 
	updateUsing: TranscriptMorphicUpdateStrategy morphicDoIt
	while: [ 10 timesRepeat: [  tr crShow:  'HelloWorld' . 1 second asDelay wait ] ].
	
will force the world to render.
