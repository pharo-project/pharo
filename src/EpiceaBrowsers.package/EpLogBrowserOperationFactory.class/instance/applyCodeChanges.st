public
applyCodeChanges

	self
		trigger: [ 
			self entries reverseDo: [:each |
				self handleErrorDuring: [ 
					each content applyCodeChange ]]] 
		with: self newApplyEvent