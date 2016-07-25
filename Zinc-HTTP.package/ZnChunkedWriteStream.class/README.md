I am ZnChunkedWriteStream, I wrap another write stream and add HTTP 1.1 chunked transfer encoding.

I write a chunk for each #nextPutAll: or #next:putAll:startingAt:
I should be wrapped in a ZnBufferedWriteStream for #next: to work.
When done, send #close or #finish to me.

Part of Zinc HTTP Components.
