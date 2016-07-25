storing
removeFileReference
	"We have to remove the file before writing new settings."
	| newFileReference |
	fileReference exists ifFalse: [ ^ self ].
	newFileReference := (fileReference withExtension: 'old.txt').
	newFileReference ensureDelete.
	fileReference copy renameTo: newFileReference basename.