importing
importFor: aRequestor logSource: logSource
	(self existsBehavior)
		ifFalse: [ self error: ('Cannot install comment in unexistent behavior {1}' format: { behaviorName asString } ) ].
	
	^self targetClass theNonMetaClass
						classComment: contents
						stamp: stamp