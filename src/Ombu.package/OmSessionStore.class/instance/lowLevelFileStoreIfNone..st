accessing
lowLevelFileStoreIfNone: aBlock
	"Needed by EpLostChangesDetector"
	
	^ currentStore ifNil: aBlock