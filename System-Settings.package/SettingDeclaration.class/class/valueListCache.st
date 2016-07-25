cache accessing
valueListCache
	^ ValueListCache ifNil: [ValueListCache := WeakKeyDictionary new]