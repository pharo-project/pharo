I'm an output device.
    Transcript show: 'hello'.

I implement TTranscript.

While ThreadSafeTranscript is threadsafe wrt stream access, the morphic code invoked by #changed: is not. So #changed: should not be sent from multiple threads (at random times) since this causes a morph's #update: method to conflict with the UI-thread running the morph's #drawOn: method in parallel. Whereas Morphic seems to assume that #update: and #drawOn: are run sequentially from the same thread.

The #step method is assumed to be invoked only from the main UI thread, so from here it is safe to send #changed: and consequently #update:. Methods #clear and #endEntry are invoked from multiple threads, so these signal to #step to call #changed: with the required parameter (#clearText and #appendEntry respectively.)

Method #contents should not directly return ==stream contents==, since even with a mutex around that, multiple calls from Morphic may unexpectedly get different results and fail.  #contents needs to return a value that is static between each #step (which is ==stepContents== that is only udpated in #step).

The ==stream reset== is left to occur from #clear being invoked from multiple threads. 

The ==stream resetContents== is moved to #step so this occurs directly after ==stepContents== is set from ==stream contents==.



