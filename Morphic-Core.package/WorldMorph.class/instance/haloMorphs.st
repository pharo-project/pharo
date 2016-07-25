world state
haloMorphs

	^ self hands collect: [ :h | h halo ] thenSelect: [ :halo | halo notNil ]