decompiler
remoteTemps: anArray
	remoteTemps := anArray.
	anArray do: [:tempNode| tempNode remoteNode: self]