initialization
optionalButtonRow
	"Answer the button row associated with a file list"

	| aRow |
	aRow := AlignmentMorph newRow beSticky.
	aRow color: Color transparent.
	aRow clipSubmorphs: true.
	aRow layoutInset: 0@0; cellInset: 6.
	aRow setNameTo: 'buttons'.
	aRow setProperty: #buttonRow toValue: true.  "Used for dynamic retrieval later on"
	^ aRow