saving
backupImageInFileNamed: aFileReference
	"Save the  the image under the given file. 
	Unlike #saveImageInFileNamed: this method continues evaluation in the original image after saving a backup copy.
	Note: This method does not save the .changes file, use #saveAs:"
	
	| imageFile previousFullImageName isNewSession |
	
	imageFile := aFileReference asFileReference.
	imageFile parent ensureCreateDirectory.
	previousFullImageName := self imagePath.
	isNewSession := false.
	
	[
		"change to the new image name "
		self
			changeImagePathTo: imageFile fullName;
			closeSourceFiles;
			openSourceFiles.  "so SNAPSHOT appears in new changes file"
		
		isNewSession := self snapshot: true andQuit: false.
	] ensure: [
		isNewSession ifFalse: [
			"unlike #saveImageInFileNamed: we do NOT continue in the new image"
			"switch back to the old image"
			self 
				changeImagePathTo: previousFullImageName;
				closeSourceFiles;
				openSourceFiles ]].
		
	^ isNewSession.