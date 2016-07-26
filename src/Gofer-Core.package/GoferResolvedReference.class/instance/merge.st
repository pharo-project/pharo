actions
merge
	"Merge-in the changes of this refence"
	Gofer new
		repository: self repository;
		package: package;
		version: self name;
		merge