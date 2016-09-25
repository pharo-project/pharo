*ScriptingExtensions
asClassIfAbsent: absentBlock
	"returns a global class with my name"
	^ self asClassInEnvironment: Smalltalk globals ifAbsent: absentBlock