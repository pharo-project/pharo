match enumeration
copy: aString translatingMatchesUsing: aBlock
	"Copy <aString>, except for the matches. For each match, evaluate <aBlock> passing the matched substring as the argument.  Expect the block to answer a String, and replace the match with the answer."

	| answer |
	answer := (String new: 40) writeStream.
	self copyStream: aString readStream to: answer translatingMatchesUsing: aBlock.
	^answer contents