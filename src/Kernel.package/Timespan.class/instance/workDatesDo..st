enumerating
workDatesDo: aBlock
 	"Exclude Saturday and Sunday"

	self do: aBlock with: start asDate when: [ :d | d dayOfWeek < 6 ]. 