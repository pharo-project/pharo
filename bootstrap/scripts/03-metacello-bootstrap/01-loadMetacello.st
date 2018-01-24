mcPackages := #(
 'ScriptingExtensions'
 'Text-Core'
 'System-FileRegistry'
 'FileSystem-Memory'
 'Regex-Core'
 'Ring-Core-Containers'
 'StartupPreferences'
 'ConfigurationCommandLineHandler-Core'
 'PragmaCollector'
 'Reflectivity'
 'System-FileRegistry'
 'MonticelloConfigurations'
 'Gofer-Core'
 'Metacello-Base'
 'Metacello-Core'
 'MonticelloFileTree-Core'
 'Metacello-MC'
 'Metacello-PharoCommonPlatform'
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

MCMethodDefinition initializersEnabled: false.

(MonticelloBootstrap inDirectory: (MCCacheRepository uniqueInstance directory))
  loadPackagesNamed: mcPackages.

MCMethodDefinition initializersEnabled: true.

RxMatcher initialize.
RxParser initialize.
RxsPredicate initialize.

MCFileTreeStCypressWriter initialize.
MCFileTreeFileSystemUtils initialize.
MCMockASubclass initialize.
MCMockClassA initialize.

MetacelloPlatform initialize.
MetacelloPharoPlatform initialize.
MetacelloPharo30Platform initialize.

STONWriter initialize.

MCGitHubRepository initialize.
MCGitBasedNetworkRepository initialize.

SystemAnnouncer restoreAllNotifications.
