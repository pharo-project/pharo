private
setForm: aForm

	self reset.
	form := aForm.
	port := GrafPort toForm: form.
