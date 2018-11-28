[ | repositoryPathString |

  Iceberg enableMetacelloIntegration: true.
  Iceberg remoteTypeSelector: #httpsUrl.

  repositoryPathString := ((Smalltalk os environment at: 'BOOTSTRAP_REPOSITORY' ifAbsent: ['.']) asFileReference / 'bootstrap' / 'src') fullName.
  Metacello new
	baseline: 'PharoBootstrapProcess';
	repository: 'filetree://', repositoryPathString;
	load.
] on: Warning do: #resume.
