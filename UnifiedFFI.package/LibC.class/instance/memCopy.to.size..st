misc
memCopy: src to: dest size: n
	^ self ffiCall: #(void *memcpy(void *dest, const void *src, size_t n))