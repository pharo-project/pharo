I manage the allocation and recycling of ByteArrays. 

For each size, I maintain up to limit instances, configurable using #limit:

I never preallocate.

I am threadsafe.

My public API consists of just 2 messages: #byteArrayOfSize:zero: and #recyle:

There is one global current instance for me, but I can be used as needed.

	ZdcByteArrayManager current limit: 4.
	
	ZdcByteArrayManager current limit: 0.