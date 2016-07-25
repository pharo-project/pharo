private
encode: string on: stream
	stream nextPutAll: (ZnResourceMetaUtils encodePercent: string safeSet: #rfc3986SafeSet encoder: ZnUTF8Encoder new)