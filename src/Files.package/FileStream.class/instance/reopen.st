open/close
reopen
	"Ensure that the receiver is open, re-open it if necessary."
	"Details: Files that were open when a snapshot occurs are no longer valid when the snapshot is resumed. This operation re-opens the file if that has happened."

	self subclassResponsibility
