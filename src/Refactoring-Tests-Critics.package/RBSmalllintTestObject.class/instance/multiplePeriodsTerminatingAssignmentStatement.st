methods
multiplePeriodsTerminatingAssignmentStatement
	| strings |

	strings := OrderedCollection..
	strings
		add: 'one';
		add: 'two'.
	^ strings.
	