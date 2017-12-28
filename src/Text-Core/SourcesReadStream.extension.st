Extension { #name : #SourcesReadStream }

{ #category : #'*Text-Core' }
SourcesReadStream >> nextChunkText [
	"Deliver the next chunk as a Text.  Decode the following ]style[ chunk if present.  Position at start of next real chunk."
	| string runsRaw strm runs peek pos |
	"Read the plain text"
	string := self nextChunk.
	
	"Test for ]style[ tag"
	pos := self position.
	peek := self skipSeparatorsAndPeekNext.
	peek = $] ifFalse: [self position: pos. ^ string asText].  "no tag"
	(self upTo: $[) = ']style' ifFalse: [self position: pos. ^ string asText].  "different tag"

	"Read and decode the style chunk"
	runsRaw := self basicNextChunk.	"style encoding"
	strm := ReadStream on: runsRaw from: 1 to: runsRaw size.
	runs := RunArray scanFrom: strm.

	^ Text basicNew setString: string setRunsChecking: runs.

]
