form-actions
submitForm

	self announce: (GLMFormSubmitted new result: self asFormValue)