column handling
columnDropUnabled
	"return true if column drop is enabled - see also #allowColumnDrop"
	^ columnDropUnabled ifNil: [columnDropUnabled := false]