instance creation
from: anInitialTarget andDispatcher: aDispatcher
	^self new
			dispatcher: aDispatcher;
			startOn: anInitialTarget;
			yourself