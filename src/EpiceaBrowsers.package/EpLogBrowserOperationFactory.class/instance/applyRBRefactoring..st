private
applyRBRefactoring: aRBRefactoring

	self 
		trigger: [
			self handleErrorDuring: [
				| aNautilusUI |
				aNautilusUI := NautilusUI on: Nautilus new.
				(NautilusRefactoring model: aNautilusUI) performRefactoring: aRBRefactoring ]]
		with: self newApplyEvent