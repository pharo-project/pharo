baselines
baseline: spec
	<baseline>
	
	spec for: #common do: [
		spec blessing: #baseline.
		spec baseline: 'Hermes' with: [ spec
			repository: 'github://tesonep/hermes:v2.1.0';
  			loads: 'ring' ].
		spec
			 package: 'Pharo30Bootstrap'.
		spec baseline: 'Ring2' with: [ spec
			repository: 'github://pavel-krivanek/Ring2:productionBootstrap/src';
  			loads: 'bootstrap' ].  			
]