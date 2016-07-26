class initialization
reInitializeDependentsFields
	"Object reInitializeDependentsFields"
	| oldFields |
	oldFields := DependentsFields.
	DependentsFields := WeakIdentityKeyDictionary new.
	oldFields keysAndValuesDo:[:obj :deps|
		deps do:[:d| obj addDependent: d]].
