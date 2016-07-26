enumerating
from: aReference detect: aBlockReturningBoolean

	^ self
		from: aReference
		detect: aBlockReturningBoolean
		ifNotFound: [ :ref | KeyNotFound signalFor: aReference ]