announcements
whenRemovedDo: aBlock
	"Culled block [ :oldValue :announcement | ]"

	self announcer when: ValueRemoved do: [ :ann | aBlock cull: ann oldValue cull: ann ]