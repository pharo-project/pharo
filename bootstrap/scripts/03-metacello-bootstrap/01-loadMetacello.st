| mcPackages |
mcPackages := #(
 'ScriptingExtensions'
 'System-FileRegistry'
 'FileSystem-Memory'
 'StartupPreferences'
 'PragmaCollector'
 'System-FileRegistry'
 'MonticelloFileTree-Core'
 'MonticelloFileTree-FileSystem-Utilities'
 'Metacello-Base'
 'Metacello-Core'
 'STON-Core'
 'Metacello-GitBasedRepository'
 'Metacello-Gitlab'
 'Metacello-GitHub'
 'Metacello-Bitbucket'
 'MetacelloCommandLineHandler-Core'
 ).

MCMethodDefinition initializersEnabled: false.

(MonticelloBootstrap inDirectory: (MCCacheRepository uniqueInstance directory))
  loadPackagesNamed: mcPackages!

MCMethodDefinition initializersEnabled: true.

"For now, it happens that the bootstrap does not caches the pragmas. This should be fixed later by reloading the packages after Metacello and Monticello are reloaded but we need them for reseting the system announcer for example."
CompiledMethod allInstancesDo: [ :m | m cachePragmas ].

MCFileTreeStCypressWriter initialize.
MCFileTreeFileSystemUtils initialize.

ConfigurationOf initialize.
MetacelloPlatform initialize.

STONWriter initialize.

MCGitHubRepository initialize.
MCGitBasedNetworkRepository initialize.

SystemAnnouncer restoreAllNotifications.

ZnMimeType initialize.

MCMethodDefinition initializersEnabled: true.
