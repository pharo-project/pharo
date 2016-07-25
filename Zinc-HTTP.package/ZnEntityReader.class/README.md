I am ZnEntityReader, I help reading ZnEntities from a stream given meta data in headers.

I deal with chunking and gzip decoding.

I have several options:
	- to read streaming entities, where the client has to do the actual reading
	- to read binary entities, where textual content is not interpreted
	- to allow reading up to end, when there is no content length set

Part of Zinc HTTP Components.