*ScriptingExtensions
asClassIfPresent: presentBlock
	"returns a global class with my name"
	^ presentBlock cull: (self asClassIfAbsent: [ ^ nil ])