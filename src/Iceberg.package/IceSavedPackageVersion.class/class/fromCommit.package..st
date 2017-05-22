instance creation
fromCommit: commit package: package
	^ self new
		commit: commit;
		package: package;
		yourself