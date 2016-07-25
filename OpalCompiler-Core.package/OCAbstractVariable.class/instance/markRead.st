read/write usage
markRead
	"reading does not change a #write, nor an #arg"
	usage ifNil: [usage := #read]