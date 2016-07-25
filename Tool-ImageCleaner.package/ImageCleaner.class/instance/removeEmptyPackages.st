cleaning
removeEmptyPackages
	| empty |
	empty := RPackageOrganizer default packages select: #isEmpty.
	empty do: #unregister.