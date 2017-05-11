instance creation
store: anOmBlockFileStore startPosition: aStartPosition endPosition: anEndPosition firstEntryReference: anOmReference

	^ self basicNew
		initializeWithStore: anOmBlockFileStore
		startPosition: aStartPosition
		endPosition: anEndPosition
		firstEntryReference: anOmReference;
		yourself
