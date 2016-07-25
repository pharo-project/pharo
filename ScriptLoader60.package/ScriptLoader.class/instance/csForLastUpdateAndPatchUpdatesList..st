elementary steps
csForLastUpdateAndPatchUpdatesList: aString
	"Creates update changeSet and pending list for continuous integration"
	| filename |
	filename := self csForLastUpdate: aString.
	self updateStageList: aString.
	^ filename 