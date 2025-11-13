| mcPackages |
mcPackages := #( 'Network-Kernel' 'Network-Protocols' 'Zinc-Resource-Meta-Core' 'Network-MIME' 'Zinc-HTTP' 'MonticelloRemoteRepositories' 'Zodiac-Core' ).

(MonticelloBootstrap inDirectory: (CommandLineArguments new optionAt: 'BOOTSTRAP_PACKAGE_CACHE_DIR' ifAbsent: [ MCCacheRepository uniqueInstance directory ]) asFileReference)
  loadPackagesNamed: mcPackages!

ZnConstants initialize.
