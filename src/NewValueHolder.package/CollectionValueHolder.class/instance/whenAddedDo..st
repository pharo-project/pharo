announcements
whenAddedDo: aBlock
	"Culled block [ :newValue :announcement | ]"

	announcer when: ValueAdded do: [ :ann | aBlock cull: ann newValue cull: ann ]