| mcPackages |
mcPackages := #(
 'Network-Kernel'
 'Network-MIME'
 'Network-Protocols'

 'Zinc-Resource-Meta-Core'
 'Zinc-HTTP'
 
 'MonticelloRemoteRepositories'

 'Zodiac-Core'
 ).

(MonticelloBootstrap inDirectory: (CommandLineArguments new optionAt: 'BOOTSTRAP_PACKAGE_CACHE_DIR' ifAbsent: [ MCCacheRepository uniqueInstance directory ]) asFileReference)
  loadPackagesNamed: mcPackages!

ZnConstants initialize.
