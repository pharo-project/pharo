actions
adopt
	(self confirm:
'Modifying ancestry can be dangerous unless you know
what you are doing.  Are you sure you want to adopt
',self version info name, ' as an ancestor of your working copy?')
		ifTrue: [self version adopt]