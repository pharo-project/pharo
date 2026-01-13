"Load the code for the Pharo bootstrap process, together with some packages to do the bootstrap"

[ | repositoryPathString |

  Iceberg enableMetacelloIntegration: true.
  Iceberg remoteTypeSelector: #httpsUrl.

  repositoryPathString := ((Smalltalk os environment at: 'BOOTSTRAP_REPOSITORY' ifAbsent: ['.']) asFileReference / 'src') fullName.

  Transcript show: '    [+] Loading tonel code to dump files for the Pharo bootstrap process from ' , repositoryPathString; cr.


"For now, it happens that we have an error from time to time when bootstraping with the next step (IceGenericError: error reading from the zlib stream).
This error should be fixed in recent Pharo versions but for now the bootstrap runs on Pharo ~10. In the meantime I'm adding a catch of this error with a retry."
[
  Metacello new
	  baseline: 'PharoBootstrapProcess';
	  repository: 'tonel://', repositoryPathString;
	  load ]
      on: IceGenericError
      do: [ :exception |
        Metacello new
	        baseline: 'PharoBootstrapProcess';
	        repository: 'tonel://', repositoryPathString;
	        load
      ].
] on: Warning do: #resume.
