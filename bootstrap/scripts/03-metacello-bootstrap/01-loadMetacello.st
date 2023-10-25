| mcPackages |
mcPackages := #(
 'ScriptingExtensions'
 'System-FileRegistry'
 'FileSystem-Memory'
 'Ring-Definitions-Containers'
 'StartupPreferences'
 'ConfigurationCommandLineHandler-Core'
 'PragmaCollector'
 'System-FileRegistry'
 'MonticelloConfigurations'
 'Gofer-Core'
 'Metacello-Base'
 'Metacello-Core'
 'MonticelloFileTree-Core'
 'Metacello-MC'
 'MonticelloFileTree-FileSystem-Utilities'
 'STON-Core'
 'Metacello-GitBasedRepository'
 'Metacello-Gitlab'
 'Metacello-GitHub'
 'Metacello-Bitbucket'
 'Metacello-Platform'
 'Metacello-ToolBox'
 'Metacello-PharoExtensions'
 'MetacelloCommandLineHandler-Core'
 ).

 "The next line is needed until the bootstrap runs on Pharo 12.
In the old export format of Monticello the 4th variable of a MCClassDefinition was a category. Now we have the package name and in the last variables we have the tag.
A hack has been added to find the category from the package but only if the package already exists in the image. Thus we need to have the packages before launching this other bootstrap."
 mcPackages do: [:package | RPackageOrganizer default ensurePackage: package ].

MCMethodDefinition initializersEnabled: false.

(MonticelloBootstrap inDirectory: (MCCacheRepository uniqueInstance directory))
  loadPackagesNamed: mcPackages!

MCMethodDefinition initializersEnabled: true.

"For now, it happens that the bootstrap does not caches the pragmas. This should be fixed later by reloading the packages after Metacello and Monticello are reloaded but we need them for reseting the system announcer for example."
CompiledMethod allInstancesDo: [ :m | m cachePragmas ].

MCFileTreeStCypressWriter initialize.
MCFileTreeFileSystemUtils initialize.

MetacelloPlatform initialize.

STONWriter initialize.

MCGitHubRepository initialize.
MCGitBasedNetworkRepository initialize.

SystemAnnouncer restoreAllNotifications.

ZnMimeType initialize.

MCMethodDefinition initializersEnabled: true.
