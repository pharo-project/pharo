initialization
initialize
	super initialize.
	announcer := Announcer new.
	announcer when: IcePackageLoaded do: [ self refresh ].
	announcer when: IceCommited do: [ self refresh ].
