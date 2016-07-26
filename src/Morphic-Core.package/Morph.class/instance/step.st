stepping and presenter
step
	"Do some periodic activity. Use startStepping/stopStepping to start and stop getting sent this message. The time between steps is specified by this morph's answer to the stepTime message.  The generic version dispatches control to the player, if any.  The nasty circumlocation about owner's transformation is necessitated by the flexing problem that the player remains in the properties dictionary both of the flex and the real morph.  In the current architecture, only the top renderer's pointer to the player should actually be honored for the purpose of firing."
