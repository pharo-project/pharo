instance creation
scanFrom: strm
	"Read a link in the funny format used by Text styles on files."

	^ self new url: (strm upTo: $;)