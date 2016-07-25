private
progressObservers
	progressObservers ifNil: [progressObservers := OrderedCollection new].
	^progressObservers