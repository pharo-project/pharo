development support
projectPackage
  MCWorkingCopy
    managersForClass: self configuration class
    do: [ :mgr | 
      | pkgSpec repo |
      pkgSpec := self packageSpec
        name: mgr packageName;
        yourself.
      mgr ancestors notEmpty
        ifTrue: [ pkgSpec file: mgr ancestors first name ].
      repo := mgr repositoryGroup repositories
        detect: [ :each | each ~~ MetacelloPlatform current defaultPackageCache ]
        ifNone: [ 
          Transcript
            cr;
            show:
                'Using cache repository for ' , self label , ' project package'.
          MetacelloPlatform current defaultPackageCache ].
      pkgSpec repository: (repo asRepositorySpecFor: self).
      ^ pkgSpec ].
  ^ nil