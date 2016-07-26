temporary-patch
fixUpdateInstancesOfSpecialClasses

	"Special classes such as compiled method and context should not update their instances when the system reloads them.
	If we do not do this, the initial Monticello reloading will fail because it will recognize that these classes changed.
	As such, it will create to instantiate new compiled methods and contexts, but this will fail because they should be instantiated in a special manner."
	CompiledMethod class compile: 'updateInstancesFrom: oldClass
	"I do nothing, I''m a context"'.
	
	Context class compile: 'updateInstancesFrom: oldClass
	"I do nothing, I''m a context"'.