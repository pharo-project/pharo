| mcPackages |
mcPackages := #(
 'ScriptingExtensions'
 'FileSystem-Memory'
 'STON-Core'
 'MonticelloFileTree-Core'
 'MonticelloFileTree-FileSystem-Utilities'
 'Metacello-Base'
 'Metacello-Core'
 'Metacello-GitBasedRepository'
 'Metacello-Gitlab'
 'Metacello-GitHub'
 'Metacello-Bitbucket'
 'Metacello-CommandLine'
 ).

(MonticelloBootstrap inDirectory: (MCCacheRepository uniqueInstance directory))
  loadPackagesNamed: mcPackages!

"For now, it happens that the bootstrap does not caches the pragmas. This should be fixed later by reloading the packages after Metacello and Monticello are reloaded but we need them for reseting the system announcer for example."
CompiledMethod allInstancesDo: [ :m | m cachePragmas ].

SystemAnnouncer restoreAllNotifications.

ZnMimeType initialize.

"The next deprecations were added in the development of Pharo14. They can be removed in the future."
MetacelloBaselineOfProjectSpec deprecatedAliases: #( #MetacelloMCBaselineOfProjectSpec ).
MetacelloConfigurationOfProjectSpec deprecatedAliases: #( #MetacelloMCConfigurationOfProjectSpec ).
MetacelloNamelessProjectSpec deprecatedAliases: #( #MetacelloMCNamelessProjectSpec ).
MetacelloBaselineProject deprecatedAliases: #( #MetacelloMCBaselineProject ).
