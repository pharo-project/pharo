drag'n'drop
dropDestinationDirectory: dest event: evt
	"Answer a FileDirectory representing the drop destination in the directory hierarchy morph dest"
	^ (dest itemFromPoint: evt position) withoutListWrapper