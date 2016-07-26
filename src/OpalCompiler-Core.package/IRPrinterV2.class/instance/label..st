private
label: seqNum

	"add tab and cr since this does not get called within interpretInstruction:"
	stream cr.  "extra cr just to space out sequences"
	stream nextPutAll: 'label: '.
	seqNum printOn: stream.
	stream cr.
