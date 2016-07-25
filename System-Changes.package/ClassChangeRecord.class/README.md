A ClassChangeRecorder keeps track of most substantive changes premissible in a project, isolated or not.

Structure:
inForce		a boolean
			Tells whether these changes are in effect.
			true for all changeSets in and above the current project.
			It should be sufficient only to record this for the changeSet
			as a whole, but this redundancy could help in error recovery.
classIsLocal	a boolean
			True if and only if this class is defined in this layer of the
			project structure.
changeTypes an identitySet
			Summarizes which changes have been made in this class.
			Values include #comment, #reorganize, #rename,
			and the four more summarized below.
thisName	a string
			Retains the class name for this layer.
priorName	a string
			Preserves the prior name.
thisComment	a text
			Retains the class comment for this layer.
priorComment	a text
			Preserves the prior comment.
thisOrganization	a classOrganizer
			Retains the class organization for this layer.
priorOrganization	a classOrganizer
			Preserves the prior organization.
thisMD	a methodDictionary
			Used to prepare changes for nearly atomic invocation
			of this layer (see below).
priorMD	a methodDictionary
			Preserves the state of an altered class as it exists in the next
			outer layer of the project structure.
methodChanges		a dictionary of classChangeRecords
			Retains all the method changes for this layer.

Four of the possible changeTypes are maintained in a mutually exclusive set, analogously to MethodChangeRecords.  Here is a simple summary of the relationship between these four changeType symbols and the recording of prior state
			|	prior == nil			|	prior not nil	
	---------	|----------------------------	|--------------------
	add		|	add					|	change
	---------	|----------------------------	|--------------------
	remove	|	addedThenRemoved	|	remove

A classChangeRecorder is notified of changes by the method
		noteMethodChange: <ClassChangeRecord>.
ClassChangeRecorders are designed to invoke a set of changes relative to the definition of a class in an prior layer.  It is important that both invocation and revocation of these changes take place in a nearly atomic fashion so that interdependent changes will be adopted as a whole, and so that only one flush of the method cache should be necessary.  A further reason for revocation to be simple is that it may be requested as an attempt to recover from an error in a project that is failing.