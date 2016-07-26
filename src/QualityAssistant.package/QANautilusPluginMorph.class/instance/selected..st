list-morph
selected: index
	| critique |

	(index == 0 or:
	[ pluginDelegate isNil ]) ifTrue: [ ^ self ].
	
	critique := (critiques at: index).
	pluginDelegate critiqueSelected: critique.
	ReSystemAnnouncer uniqueInstance
		notifyCritique: critique
		clickedOnFor: entity.