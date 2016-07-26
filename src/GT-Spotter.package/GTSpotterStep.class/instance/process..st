processing
process: aString
	"I do not accept empty queries or the same queries as before.
	If nil is passed, than an empty string will be processed. This approach is used to
	destinguish when user deletes last character in the search and when search field became
	empty after spawning new step"
	
	"no need to search for the same query"
	(self context matches: aString) ifTrue: [ ^ self ].
	self context: (self newContextForQuery: aString).
	
	" updating timestamp in a stream, to prevent candidates from old process being shown "
	self stream updateTimestamp.
	
	self asyncTask
		execute: [ :aCurrentContext |
			self spotter notifyContextChangedFrom: lastSearchContext to: self context.
			self processContext: aCurrentContext ]
		withArguments: { self context. }
		callback: [
			lastSearchContext := self context. 
			self spotter notifySearchFinishedFor: aString processors: self processors ]
		"added small delay before actual searching. It makes spotter faster :)"
		delay: 200