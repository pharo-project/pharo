accessing
content
	^ (self prim_content getHandle copyFrom: 1 to: self prim_content_len) asString