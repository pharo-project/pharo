I am ZdcSocketStream.

I am a ZdcOptimizedSocketStream.

I further optimize my superclass' methods dealing with bulk input and output
to transparently bypass the internal buffers when this makes sense.