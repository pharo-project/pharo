removal
isClassRemoval
	"NOTE: there are other removals with changeType #addedThenRemoved,
	but this message is used to write out removals in fileOut, and those
	cases should not be written out."

	^ (changeTypes includes: #remove) or: [changeTypes includes: #removeClass]