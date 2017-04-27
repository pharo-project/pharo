public
revertCodeChangesInPreviewMode

	self
		trigger: [ 
			self entries reverseDo: [:each |
				self handleErrorDuring: [ 
					each content applyCodeChange ]]] 
		with: self newRevertEvent