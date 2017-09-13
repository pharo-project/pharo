accessing
rationale

	^ 'You should override on the CLASS SIDE at least one of #',
	  (self complianceMethods joinUsing: ', #'),
	  ' methods to return true and so indicate which entities can be checked by your rule.'