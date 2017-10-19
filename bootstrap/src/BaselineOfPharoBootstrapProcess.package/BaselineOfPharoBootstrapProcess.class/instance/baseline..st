baselines
baseline: spec
	<baseline>
	
	spec for: #common do: [
		spec blessing: #baseline.
		spec baseline: 'Hermes' with: [ spec
			repository: 'github://tesonep/hermes:v2.2.1';
  			loads: 'ring' ].
		spec
			 package: 'Pharo30Bootstrap'.
		spec baseline: 'Ring2' with: [ spec
			repository: 'github://pavel-krivanek/Ring2:v1.0.2/src';
  			loads: 'bootstrap' ].  			
]