I am ZdcSecureSocketStream, a binary read/write stream for SSL communication.

I am a ZdcOptimizedSocketStream.

When I am used as a client, call #connect on me before using me as a normal stream.

When I am used as a server, call #accept on me before using me as a normal stream.

Currently, certificate management is ignored.
