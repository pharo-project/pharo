instance creation
scanFrom: strm
	"read a link in the funny format used by Text styles on files. Rhttp://www.disney.com;"

	^ self new url: (strm upTo: $;)