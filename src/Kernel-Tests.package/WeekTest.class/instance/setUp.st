running
setUp
	"June 1998, 5th week"

	super setUp.
	restoredStartDay := Week startDay.
	Week startDay: #Sunday.
	week := Week starting: '4 July 1998' asDate