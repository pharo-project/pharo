I am ZnStreamingEntity, a concrete HTTP Entity based on a stream.
I am a ZnEntity.

When a ZnStreamingEntity is created using #readFrom: 
its content type and length are set but no body is actually read.
Instead, stream is set to a ZnLimitedReadStream that clients can/must
use to read from.

When the client is done, either all data must be read or #consumeContent must be called.
A client should normally not close the stream.

A ZnStreamingEntity can also be instanciated with an existing read stream.
During #writeOn: this stream is copied to the output stream.

Part of Zinc HTTP Components.