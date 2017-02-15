I am special failure which signalled that some processes was forked during tests and was failed.
I am resumable and resume will open debuggers on failed processes (in fact they will be resumed).

I mark tests red when they produce such processes. So no green tests which spawn "background debuggers".

Also I am signalled first when test itself is failed. By resuming it debugger will be opened on test too together with failed processes 