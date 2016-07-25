I am ZnOptimizedSocketStream.

I am a ZdcSimpleSocketStream.

I re-implement the critical operations dealing with bulk input and output 
more efficiently to work with buffer sized chunks, bypassing #next and #nextPut: