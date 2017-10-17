announcements
whenRemovedDo: aBlock
	"Culled block [ :oldValue :announcement | ]"

	announcer when: ValueRemoved do: [ :ann | aBlock cull: ann oldValue cull: ann ]