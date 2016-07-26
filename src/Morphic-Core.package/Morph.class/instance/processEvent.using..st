events-processing
processEvent: anEvent using: defaultDispatcher
	"This is the central entry for dispatching events in morphic. Given some event and a default dispatch strategy, find the right receiver and let him handle it.
	WARNING: This is a powerful hook. If you want to use a different event dispatcher from the default, here is the place to hook it in. Depending on how the dispatcher is written (e.g., whether it calls simply #processEvent: or #processEvent:using:) you can change the dispatch strategy for entire trees of morphs. Similarly, you can disable entire trees of morphs from receiving any events whatsoever. Read the documentation in class MorphicEventDispatcher before playing with it. "
	(self rejectsEvent: anEvent) ifTrue:[^#rejected].
	^defaultDispatcher dispatchEvent: anEvent with: self