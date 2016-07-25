all changes
trimHistory
	"Drop non-essential history."

	"Forget methods added and later removed"
	methodChanges keysAndValuesRemove:
		[:sel :chgRecord | chgRecord changeType == #addedThenRemoved].

	"Forget renaming and reorganization of newly-added classes."
	(changeTypes includes: #add) ifTrue:
		[changeTypes removeAllFoundIn: #(rename reorganize)].
