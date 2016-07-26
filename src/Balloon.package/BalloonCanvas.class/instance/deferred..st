accessing
deferred: aBoolean
	deferred == aBoolean ifTrue:[^self].
	self flush. "Force pending prims on screen"
	deferred := aBoolean.
	engine ifNotNil:[engine deferred: aBoolean].