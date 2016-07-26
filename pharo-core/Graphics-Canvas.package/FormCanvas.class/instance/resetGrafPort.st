private
resetGrafPort
	"Private! Create a new grafPort for a new copy."

	port := GrafPort toForm: form.
	port clipRect: clipRect.
