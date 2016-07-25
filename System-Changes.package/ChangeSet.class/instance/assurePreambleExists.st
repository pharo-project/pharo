filein/out
assurePreambleExists
	"Make sure there is a StringHolder holding the preamble; if it's found to have reverted to empty contents, put up the template"

	(preamble isEmptyOrNil)
		ifTrue: [preamble := self preambleTemplate]