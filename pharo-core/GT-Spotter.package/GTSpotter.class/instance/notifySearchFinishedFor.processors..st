notifying
notifySearchFinishedFor: aString processors: aCollectionOfProcessors

	self announce: (GTSpotterSearchFinished new 
								query: aString;
								processors: aCollectionOfProcessors;
								yourself)