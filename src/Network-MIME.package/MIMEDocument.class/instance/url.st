accessing
url
	"Answer the URL the receiver was downloaded from.  It may legitimately be nil."

	^ uri ifNotNil:[uri asString asUrl]