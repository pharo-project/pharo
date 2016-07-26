initialization
initialize 
	"Initialize the receiver to be empty."

	super initialize.
	name ifNil: [^ self error: 'All changeSets must be registered, as in ChangeSorter newChangeSet'].
	self clear.
