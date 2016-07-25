monticello menus behavior
open: mcpackage in: repo 
	| workingCopy |

	repo ifNil: [ ^ self ].
	workingCopy := mcpackage workingCopy.
	repo morphicOpen: workingCopy