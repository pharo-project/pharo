initialization
folderSelectButtonRow
	"Answer the button row used for folder selection"

	| aRow |
	aRow := AlignmentMorph newRow beSticky.
	aRow color: Color transparent.
	aRow clipSubmorphs: true.
	aRow layoutInset: 0@0; cellInset: 6.
	aRow setNameTo: 'buttons'.
	^ aRow