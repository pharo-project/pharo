*renraku
critiques
	<eProperty>
	| critiques |
	
	critiques := ReCriticEngine critiquesOf: self.
	critiques :=
		critiques,
		(ReCriticEngine nodeCritiquesOf: self).
	
	^ critiques