patches
snapshot
	^ [ self mcVersion snapshot ] on: IceVersionDoesNotExist do: [ MCSnapshot empty ]