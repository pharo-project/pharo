running
ensureChangesFileOpenedInProcess
	"Ensure the changes file is open"

	| remoteString |
	remoteString := SourceFiles remoteStringAt: thisContext method sourcePointer.

	self deny: remoteString string isNil.
	"self deny: ProcessAndSessionLocalSourcesFileArray value isNil."