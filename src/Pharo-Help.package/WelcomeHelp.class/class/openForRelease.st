showing
openForRelease
	<script>
	| browser window findBlock |
	
	findBlock := [ :each | (each isKindOf: SystemWindow) and: [ each label = self bookName ] ]. 
	World submorphs 
		detect: findBlock 
		ifFound: [ :oldWindow | oldWindow delete ].
	browser := self open.
	window := browser dependents detect: findBlock.
	window extent: 700@400.
	window center: Display extent / 2