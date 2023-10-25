| mcPackages |
mcPackages := #(
 'Network-Kernel'
 'Network-MIME'
 'Network-Protocols'

 'MonticelloRemoteRepositories'

 'Zinc-HTTP'
 'Zodiac-Core'
 ).

"The next line is needed until the bootstrap runs on Pharo 12.
In the old export format of Monticello the 4th variable of a MCClassDefinition was a category. Now we have the package name and in the last variables we have the tag.
A hack has been added to find the category from the package but only if the package already exists in the image. Thus we need to have the packages before launching this other bootstrap."
 mcPackages do: [:package | RPackageOrganizer default ensurePackage: package ].

(MonticelloBootstrap inDirectory: (CommandLineArguments new optionAt: 'BOOTSTRAP_PACKAGE_CACHE_DIR' ifAbsent: [ MCCacheRepository uniqueInstance directory ]) asFileReference)
  loadPackagesNamed: mcPackages!

InternetConfiguration initialize.
NetNameResolver initialize.
Socket initialize.
Base64MimeConverter initialize.

ZnBase64Encoder initialize.
ZnByteEncoder initialize.
ZnUTF8Encoder initialize.
ZnLogEvent initialize.
ZnConstants initialize.
ZnHeaders initialize.
ZnNetworkingUtils initialize.
ZnServer initialize.
ZnSingleThreadedServer initialize.
