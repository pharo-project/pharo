as yet unclassified
scanFrom: strm
	"read a link in the funny format used by Text styles on files. LPoint +;LPoint Comment;"

	^ self new classAndMethod: (strm upTo: $;)