cleanup
cleanUp: aggressive except: exclusions
	"Clean up. When aggressive is true, this will destroy resources, change sets, etc.
	Leave out any classes specifically listed in exclusions."

	"Smalltalk cleanUp: true except: {Point . ChangeSet}"

	^self cleanUp: aggressive except: exclusions confirming: aggressive