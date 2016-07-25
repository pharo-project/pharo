accessing
title
	^ 'Synchronizing {1} -- branch: {2} -- origin: {3}' 
		format: {
			self repository location gtDisplayString.
			self repository branch.
			self repository origin }
		