initialization
registerEventsForShout

	aboutToStyleHolder whenChangedDo: [ self changed: #getText ].
	behaviorHolder
		whenChangedDo: [ :class | 
			self changed: #classOrMetaClass: with: { class }.
			self changed: #getText ]