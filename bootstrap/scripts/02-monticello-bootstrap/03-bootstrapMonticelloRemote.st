mcPackages := #(
 'Network-Kernel'
 'Network-MIME'
 'Network-Protocols'

 'MonticelloRemoteRepositories'

 'Zinc-HTTP'
 'Zinc-FileSystem'
 'Zodiac-Core'
 ).

(MonticelloBootstrap inDirectory: (MCCacheRepository uniqueInstance directory))
  loadPackagesNamed: mcPackages.

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
