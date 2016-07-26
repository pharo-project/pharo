exception handling
allowEvenIfLocked
  "for internal ScriptEngine use only. from the Scripting API use:
		onLocked: [:ex | ex allow]
		"

  self resume: self newProjectRegistration