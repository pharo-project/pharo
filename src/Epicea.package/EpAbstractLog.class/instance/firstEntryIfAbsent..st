accessing
firstEntryIfAbsent: absentBlock
	"Answer the first entry of the log, or evaluate the absentBlock."

	^ self entries ifEmpty: absentBlock ifNotEmpty: [:collection | collection first ]