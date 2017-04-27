public
revertCodeChanges

	self
		trigger: [ 
			self entries do: [:each | 
				self handleErrorDuring: [ 
					each content asRevertedCodeChange applyCodeChange ]]]
		with: self newRevertEvent