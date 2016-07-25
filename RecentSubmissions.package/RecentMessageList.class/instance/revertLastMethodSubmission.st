actions
revertLastMethodSubmission
	"If the most recent method submission was a method change, revert
	that change, and if it was a submission of a brand-new method, 
	remove that method."

	"self new revertLastMethodSubmission"

	| changeRecords lastSubmission theClass theSelector |
	self methodReferenceList isEmpty ifTrue: [ ^ self ].
	lastSubmission := self lastEntry.
	theClass := lastSubmission actualClass ifNil: [ ^ self ].
	theSelector := lastSubmission selector.
	changeRecords := SourceFiles changeRecordsFor: lastSubmission.
	changeRecords isEmptyOrNil ifTrue: [ ^ self ].
	changeRecords size = 1
		ifTrue: [ theClass removeSelector: theSelector ] "method has no prior version, so reverting in this case means removing"
		ifFalse: [ changeRecords second fileIn ]