scripting
unmarkAsModified: somePackageNames
	"HACK: Unmark as modified the foreign packages where Epicea overrides methods. Not needed anymoew: I discovered that the same effect can be obtained just suffixing the protocols with -override."

	somePackageNames do: [ :name |
		(self mcWorkingCopyOf: name) modified: false ]