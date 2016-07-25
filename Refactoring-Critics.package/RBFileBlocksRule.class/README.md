Checks assignment to a variable that is the first statement inside the value block that is also used in the unwind block. For example

| inputStream  |
    [
    inputStream := self openStream. "may fail !"
    inputStream doSomeOperation. "may fail !"
    ] ensure: [ inputStream ifNotNil: [ inputStream close ]].

 if "openStream" fails, we don't need to ensure the stream is closed.
And otherwise, if we move the assignment outside of the block, we don't need
an "ifNotNil"-check for the ensure block if "doSomeOperation" fails.
This code can be changed to

| inputStream  |
    inputStream := self openStream. "may fail !"
    [inputStream doSomeOperation. "may fail !"
    ] ensure: [ inputStream close ].