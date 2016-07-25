update
updateSelectedFile
	"Notify that the selected file has changed."

	self
		changed: #selectedFileIndex;
		changed: #selectedFileEntry;
		changed: #selectedFileName;
		changed: #selectedPathName;
		changed: #okEnabled;
		changed: #hasSelectedFileOrDirectory.
	self updatePreview