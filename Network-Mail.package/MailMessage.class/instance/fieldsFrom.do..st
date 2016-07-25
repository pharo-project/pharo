parsing
fieldsFrom: aStream do: aBlock 
	"Invoke the given block with each of the header fields from the given stream. The block arguments are the field name and value. The streams position is left right after the empty line separating header and body."
	| savedLine line s |
	savedLine := self readStringLineFrom: aStream.
	[ aStream atEnd ] whileFalse: 
		[ line := savedLine.
		line isEmpty ifTrue: [ ^ self ].	"quit when we hit a blank line"
		
		[ savedLine := self readStringLineFrom: aStream.
		savedLine notEmpty and: [ savedLine first isSeparator ] ] whileTrue: 
			[ "lines starting with white space are continuation lines"
			s := savedLine readStream.
			s skipSeparators.
			line := line , ' ' , s upToEnd ].
		self 
			reportField: line trimBoth
			to: aBlock ].

	"process final header line of a body-less message"
	savedLine isEmpty ifFalse: 
		[ self 
			reportField: savedLine trimBoth
			to: aBlock ]