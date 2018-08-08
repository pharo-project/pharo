baselines
baseline: spec
	<baseline>
	
	spec for: #common do: [
		spec blessing: #baseline.
		spec baseline: 'Tonel' with: [ spec
			repository: 'github://pharo-vcs/tonel:v1.0.9' ].
		spec baseline: 'Hermes' with: [ spec
			repository: 'github://tesonep/hermes:v2.4.1';
  			loads: 'ring' ].
		spec
			 package: 'Pharo30Bootstrap'.
		spec baseline: 'Ring2' with: [ spec
			repository: 'github://pavel-krivanek/Ring2:7edb3edccb151d59c244bc470ad3770b24d36e62/src';
  			loads: 'bootstrap' ].  			
]