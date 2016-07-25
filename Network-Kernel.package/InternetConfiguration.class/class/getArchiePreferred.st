lookups
getArchiePreferred
	"Return the preferred Archie server"
	"InternetConfiguration getArchiePreferred"

	^self primitiveGetStringKeyedBy: 'ArchiePreferred'
