installing
installStableVersion 
	  Metacello new
		repository: self repositoryUrl, '/',self name;
		configuration: self name;
		version: #'stable';
		load