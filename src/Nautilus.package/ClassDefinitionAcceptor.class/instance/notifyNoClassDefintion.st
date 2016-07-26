private
notifyNoClassDefintion
	"if this was not a class definition, the user has tried to
	define a method without selecting a class"

	^ UIManager default alert: 'No class selected'