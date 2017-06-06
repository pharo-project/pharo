opening
openForRelease
	<script>
	| window |
	
	World submorphs 
		detect: [ :each | (each isKindOf: SystemWindow) and: [ each label = self title ] ] 
		ifFound: [ :oldWindow | oldWindow delete ].
	window := self open.
	window extent: 750@400.
	window center: Display extent / 2