saving
saveAsNewVersion
	"Save the image/changes using the next available version number."
	"SmalltalkImage current saveAsNewVersion"
	
	| newImageFile newChangesFile strippedName |
	newImageFile := self imageFile nextVersion.
	newChangesFile := newImageFile withExtension: self changesSuffix.

	newChangesFile exists ifTrue:
		[^self inform:
'There is already .changes file of the desired name,
', newChangesFile fullName, '
curiously already present, even though there is
no corresponding .', self imageSuffix,' file.   Please remedy
manually and then repeat your request.'].

	strippedName := self stripImageExtensionFrom: newImageFile fullName.
	^self saveAs: strippedName