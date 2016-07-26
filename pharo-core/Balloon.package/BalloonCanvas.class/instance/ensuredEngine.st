accessing
ensuredEngine
	engine ifNil:[
		engine := BalloonEngine new.
		"engine := BalloonDebugEngine new"
		engine aaLevel: aaLevel.
		engine bitBlt: port.
		engine destOffset: origin.
		engine clipRect: clipRect.
		engine deferred: deferred.
		engine].
	engine colorTransform: colorTransform.
	engine edgeTransform: transform.
	^engine