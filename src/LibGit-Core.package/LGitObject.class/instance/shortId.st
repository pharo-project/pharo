accessing
shortId
	| buf shortId |
	buf := LGitBuf new.
	self object_short_id: buf.
	shortId := buf prim_ptr readString.
	buf free.
	^shortId 