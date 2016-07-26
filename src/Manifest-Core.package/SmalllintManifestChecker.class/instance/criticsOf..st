accessing
criticsOf: aRule

	| rules |	
	rules := rule leaves 
		detect: [:r | r class = aRule class]
		ifNone: [ ^ #() ].
	^ rules critics