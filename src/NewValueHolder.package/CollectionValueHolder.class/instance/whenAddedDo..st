announcements
whenAddedDo: aBlock
	"Culled block [ :newValue :announcement | ]"

	self announcer when: ValueAdded do: [ :ann | aBlock cull: ann newValue cull: ann ]